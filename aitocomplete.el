(require 'json)

(defgroup aitocomplete nil
  "Ollama completion interface for Emacs."
  :group 'applications
  :prefix "aitocomplete-")

(defcustom aitocomplete-model "llama3.2"
  "Default Ollama model to use."
  :type 'string
  :group 'aitocomplete)


(global-set-key (kbd "C-c s") #'aitocomplete-send-region)

(defvar aitocomplete-chat-buffer "*AI Chat*"
  "Buffer name for Ollama chat interactions.")

(defvar-local aitocomplete--current-response ""
  "Accumulated response for current LLM query.")

(defvar-local aitocomplete--response-buffer ""
  "Buffer to accumulate partial JSON responses.")

(defvar-local aitocomplete--accumulated-content ""
  "Accumulated content from all response chunks.")

;; menu items
(defcustom aitocomplete-menu-columns 3
  "Number of columns to display in the menu."
  :type 'integer
  :group 'aitocomplete)

(defcustom aitocomplete-menu-items
  '((?o . ("Open chat buffer" . aitocomplete-chat))
    (?s . ("Send region" . aitocomplete-send-region))
    (?m . ("Change model" . 
           (lambda ()
             (let ((new-model (completing-read 
                              "Select model: " 
                              '("llama2" "codellama" "mistral" "neural-chat")
                              nil t)))
               (setq aitocomplete-model new-model)
               (message "Model changed to: %s" new-model)))))
    (?t . ("Test response" . aitocomplete-test-response))
    (?q . ("Quit" . (lambda () (message "Quit menu.")))))
  "Menu items for aitocomplete"
  :type '(alist :key-type character
                :value-type (cons string function))
  :group 'aitocomplete)

(define-derived-mode aitocomplete-chat-mode text-mode "AI Chat"
  "Major mode for Ollama chat interaction."
  (setq-local word-wrap t)
  (visual-line-mode 1)
  (use-local-map aitocomplete-chat-mode-map))

(defun aitocomplete--server-running-p ()
  "Check if Ollama server is running."
  (= 0 (call-process "curl" nil nil nil 
                     "--silent" 
                     "--fail" 
                     "http://localhost:11434/api/tags")))

(defun aitocomplete-chat ()
  "Open or switch to the AI chat buffer."
  (interactive)
  (let ((buf (get-buffer-create aitocomplete-chat-buffer)))
    (with-current-buffer buf
      (unless (eq major-mode 'aitocomplete-chat-mode)
        (aitocomplete-chat-mode))
      (when (= (buffer-size) 0)
        (insert "Welcome to AI Chat!\n")
        (insert (format "Currently using model: %s\n\n" aitocomplete-model))
        (insert "Type your message and press C-c C-c to send.\n")
        (insert "----------------------------------------\n\n")))
    (display-buffer buf 
                    '((display-buffer-in-side-window)
                      (side . right)
                      (window-width . 50)))))

(defun aitocomplete-send-region ()
  "Send the selected region to the AI chat buffer."
  (interactive)
  (unless (use-region-p)
    (user-error "No region selected"))
  
  ;; Get the selected text and print debug info
  (let* ((text (buffer-substring-no-properties 
                (region-beginning) 
                (region-end)))
         (source-buffer (buffer-name)))
    
    (message "Debug: Selected text length: %d" (length text))
    
    ;; Create or get the chat buffer
    (let ((chat-buf (get-buffer-create aitocomplete-chat-buffer)))
      ;; Switch to the chat buffer
      (with-current-buffer chat-buf
        (message "Debug: Switched to buffer: %s" (buffer-name))
        
        ;; Ensure we're in aitocomplete-chat-mode
        (unless (eq major-mode 'aitocomplete-chat-mode)
          (aitocomplete-chat-mode))
        
        ;; Go to the end and ensure we're on a fresh line
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        
        ;; Insert the text on its own line
        (let ((start-pos (point)))
          (insert text)
          
          ;; Ensure text ends with newline
          (unless (bolp) (insert "\n"))
          
          ;; Go back to the start of our inserted text
          (goto-char start-pos)
          
          (message "Debug: Text at point: '%s'" 
                  (buffer-substring-no-properties
                   (line-beginning-position)
                   (line-end-position)))
          
          ;; Now send this line
          (aitocomplete-send-message)))
      
      ;; Show the chat buffer
      (display-buffer chat-buf 
                      '((display-buffer-in-side-window)
                        (side . right)
                        (window-width . 80))))))


(defun aitocomplete--process-filter (proc output)
  "Process filter for Ollama responses. Only prints once at completion."
  (when-let ((buf (process-buffer proc)))
    (with-current-buffer buf
      ;; Accumulate output
      (setq aitocomplete--response-buffer 
            (concat aitocomplete--response-buffer output))
      
      ;; Process complete JSON objects
      (while (string-match "\\`[[:space:]]*{.+?}[[:space:]]*\\(\n\\|\\'\\)" 
                          aitocomplete--response-buffer)
        (let* ((json-object-end (match-end 0))
               (json-str (substring aitocomplete--response-buffer 0 json-object-end)))
          ;; Remove processed JSON from buffer
          (setq aitocomplete--response-buffer 
                (substring aitocomplete--response-buffer json-object-end))
          
          ;; Process the JSON object
          (when-let* ((json-data (ignore-errors (json-read-from-string json-str)))
                     (msg (alist-get 'message json-data))
                     (content (alist-get 'content msg)))
            
            ;; Check if this is the final message
            (if (eq (alist-get 'done json-data) t)
                ;; On completion, print the full accumulated content
                (save-excursion
                  (goto-char (point-max))
                  ;; Replace "thinking" message with full response
                  (when (search-backward "AI is thinking..." nil t)
                    (let ((inhibit-read-only t))
                      (delete-region (line-beginning-position) (line-end-position))
                      (insert "AI: " aitocomplete--accumulated-content))))
              ;; Otherwise just accumulate content
              (setq aitocomplete--accumulated-content
                    (concat aitocomplete--accumulated-content content)))))))))

(defun aitocomplete-send-message ()
  "Send the current message to Ollama."
  (interactive)
  (if (not (aitocomplete--server-running-p))
      (message "Error: Ollama server is not running!")
    (let* ((message-text (string-trim
                         (buffer-substring-no-properties
                          (line-beginning-position)
                          (line-end-position)))))
      
      (message "Debug: Message text length: %d" (length message-text))
      
      (when (string-empty-p message-text)
        (user-error "No message to send"))
      
      (let* ((json-payload (json-encode
                           `(("model" . ,aitocomplete-model)
                             ("messages" . 
                              [(("role" . "user")
                                ("content" . ,message-text))]))))
             (proc (start-process-shell-command
                   "ollama-chat" (current-buffer)
                   (format "curl -s -X POST http://localhost:11434/api/chat -H 'Content-Type: application/json' -d '%s'"
                          (replace-regexp-in-string "'" "\\\\\"" json-payload)))))
        
        ;; Reset response buffers
        (setq-local aitocomplete--response-buffer "")
        (setq-local aitocomplete--accumulated-content "")
        (insert "\n\nAI is thinking...\n")
        (set-process-filter proc #'aitocomplete--process-filter)
        (set-process-sentinel 
         proc
         (lambda (proc event)
           (when (string-match-p "finished" event)
             (with-current-buffer (process-buffer proc)
               (goto-char (point-max))
               (insert "\n----------------------------------------\n\n")))))))))

(defun aitocomplete-test-response ()
  "Test function to examine Ollama JSON responses."
  (interactive)
  (let* ((json-payload (json-encode
                       `(("model" . ,aitocomplete-model)
                         ("messages" . 
                          [(("role" . "user")
                            ("content" . "Say hello!"))]))))
         (buf (get-buffer-create "*AI Test*"))
         proc)
    ;; Clear the test buffer
    (with-current-buffer buf
      (erase-buffer)
      (insert "Starting AI test...\n\n"))
    
    ;; Create process and set up filter
    (setq proc (start-process-shell-command
                "ollama-test" buf
                (format "curl -s -X POST http://localhost:11434/api/chat -H 'Content-Type: application/json' -d '%s'"
                        (replace-regexp-in-string "'" "\\\\\"" json-payload))))
    
    ;; Set up process filter to examine JSON
    (set-process-filter 
     proc
     (lambda (proc output)
       (with-current-buffer (process-buffer proc)
         (goto-char (point-max))
         (insert "\n--- New chunk received ---\n")
         (insert output)
         ;; Try to parse and examine each line
         (dolist (line (split-string output "\n" t))
           (when-let ((json-obj (ignore-errors (json-read-from-string line))))
             (insert "\nParsed JSON object:\n")
             (pp json-obj (current-buffer)))))))
    
    ;; Display the buffer
    (pop-to-buffer buf)))

;; menu 
(defun aitocomplete--format-menu (items columns)
  "Format ITEMS into COLUMNS columns for display."
  (let* ((items-list (mapcar (lambda (item) 
                              (format "[%c] %s" 
                                     (car item) 
                                     (cadr item)))
                            items))
         (total (length items-list))
         (rows (ceiling (/ total (float columns))))
         (padded-list (append items-list 
                             (make-list (- (* rows columns) total) "")))
         (format-string
          (concat
           (mapconcat
            (lambda (width) (format "%%-%ds" (+ width 2)))
            (butlast
             (cl-loop for col below columns collect
                      (cl-loop for row below rows
                               for idx = (+ (* col rows) row)
                               when (< idx total)
                               maximize (length (nth idx padded-list)))))
            "")
           "%s")))
    (format "\nAItoComplete Menu | Server: %s | Model: %s\n%s\n%s"
            (if (aitocomplete--server-running-p)
                "RUNNING"
              "NOT RUNNING")
            (or aitocomplete-model "none")
            (make-string 50 ?-)
            (mapconcat
             (lambda (row)
               (apply 'format format-string row))
             (cl-loop for row below rows collect
                      (cl-loop for col below columns
                               for idx = (+ (* col rows) row)
                               when (< idx (length padded-list))
                               collect (nth idx padded-list)))
             "\n"))))

(defun aitocomplete-menu ()
  "Display the aitocomplete menu."
  (interactive)
  (let* ((prompt (propertize 
                  (aitocomplete--format-menu 
                   aitocomplete-menu-items 
                   aitocomplete-menu-columns)
                  'face 'minibuffer-prompt))
         (key (read-key prompt))
         (cmd (assoc key aitocomplete-menu-items)))
    (when cmd
      (funcall (cdr (cdr cmd))))))


(defvar aitocomplete-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'aitocomplete-send-message)
    (define-key map (kbd "?") #'aitocomplete-menu)
    map)
  "Keymap for AI chat mode.")


(provide 'aitocomplete)
