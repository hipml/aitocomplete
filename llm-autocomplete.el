(require 'json)

(defgroup ollama-complete nil
  "Ollama completion interface for Emacs."
  :group 'applications
  :prefix "ollama-complete-")

(defcustom ollama-complete-model "llama3.2"
  "Default Ollama model to use."
  :type 'string
  :group 'ollama-complete)

(defvar ollama-complete-chat-buffer "*Ollama Chat*"
  "Buffer name for Ollama chat interactions.")

(defvar-local ollama-complete--current-response ""
  "Accumulated response for current LLM query.")

(defvar-local ollama-complete--response-buffer ""
  "Buffer to accumulate partial JSON responses.")

(defvar-local ollama-complete--accumulated-content ""
  "Accumulated content from all response chunks.")

(defun ollama-complete--server-running-p ()
  "Check if Ollama server is running."
  (= 0 (call-process "curl" nil nil nil 
                     "--silent" 
                     "--fail" 
                     "http://localhost:11434/api/tags")))

(defun ollama-complete-chat ()
  "Open or switch to the Ollama chat buffer."
  (interactive)
  (let ((buf (get-buffer-create ollama-complete-chat-buffer)))
    (with-current-buffer buf
      (unless (eq major-mode 'ollama-complete-chat-mode)
        (ollama-complete-chat-mode))
      (when (= (buffer-size) 0)
        (insert "Welcome to Ollama Chat!\n")
        (insert (format "Currently using model: %s\n\n" ollama-complete-model))
        (insert "Type your message and press C-c C-c to send.\n")
        (insert "----------------------------------------\n\n")))
    (display-buffer buf 
                    '((display-buffer-in-side-window)
                      (side . right)
                      (window-width . 50)))))

(defvar ollama-complete-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'ollama-complete-send-message)
    map)
  "Keymap for Ollama chat mode.")

(define-derived-mode ollama-complete-chat-mode text-mode "Ollama Chat"
  "Major mode for Ollama chat interaction."
  (setq-local word-wrap t)
  (visual-line-mode 1))

(defun ollama-complete--process-filter (proc output)
  "Process filter for Ollama responses. Only prints once at completion."
  (when-let ((buf (process-buffer proc)))
    (with-current-buffer buf
      ;; Accumulate output
      (setq ollama-complete--response-buffer 
            (concat ollama-complete--response-buffer output))
      
      ;; Process complete JSON objects
      (while (string-match "\\`[[:space:]]*{.+?}[[:space:]]*\\(\n\\|\\'\\)" 
                          ollama-complete--response-buffer)
        (let* ((json-object-end (match-end 0))
               (json-str (substring ollama-complete--response-buffer 0 json-object-end)))
          ;; Remove processed JSON from buffer
          (setq ollama-complete--response-buffer 
                (substring ollama-complete--response-buffer json-object-end))
          
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
                  (when (search-backward "Ollama is thinking..." nil t)
                    (let ((inhibit-read-only t))
                      (delete-region (line-beginning-position) (line-end-position))
                      (insert "Ollama: " ollama-complete--accumulated-content))))
              ;; Otherwise just accumulate content
              (setq ollama-complete--accumulated-content
                    (concat ollama-complete--accumulated-content content)))))))))

(defun ollama-complete-send-message ()
  "Send the current message to Ollama."
  (interactive)
  (if (not (ollama-complete--server-running-p))
      (message "Error: Ollama server is not running!")
    (let* ((message-text (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position)))
           (json-payload (json-encode
                         `(("model" . ,ollama-complete-model)
                           ("messages" . 
                            [(("role" . "user")
                              ("content" . ,message-text))]))))
           (proc (start-process-shell-command
                 "ollama-chat" (current-buffer)
                 (format "curl -s -X POST http://localhost:11434/api/chat -H 'Content-Type: application/json' -d '%s'"
                        (replace-regexp-in-string "'" "\\\\\"" json-payload)))))
      ;; Reset ALL response buffers
      (setq-local ollama-complete--response-buffer "")
      (setq-local ollama-complete--accumulated-content "")
      (insert "\n\nOllama is thinking...\n")
      (set-process-filter proc #'ollama-complete--process-filter)
      (set-process-sentinel 
       proc
       (lambda (proc event)
         (when (string-match-p "finished" event)
           (with-current-buffer (process-buffer proc)
             (goto-char (point-max))
             (insert "\n----------------------------------------\n\n"))))))))

(defun ollama-complete-test-response ()
  "Test function to examine Ollama JSON responses."
  (interactive)
  (let* ((json-payload (json-encode
                       `(("model" . ,ollama-complete-model)
                         ("messages" . 
                          [(("role" . "user")
                            ("content" . "Say hello!"))]))))
         (buf (get-buffer-create "*Ollama Test*"))
         proc)
    ;; Clear the test buffer
    (with-current-buffer buf
      (erase-buffer)
      (insert "Starting Ollama test...\n\n"))
    
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


(provide 'ollama-complete)
