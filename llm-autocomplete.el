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
  "Process filter for Ollama responses."
  (when-let* ((buf (process-buffer proc)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        ;; Remove "Ollama is thinking..." and insert prefix on first response
        (when (and (string-match-p "{" output)  ; First JSON response
                   (save-excursion 
                     (search-backward "Ollama is thinking..." nil t)))
          (let ((inhibit-read-only t))
            (delete-region (line-beginning-position) (line-end-position))
            (insert "Ollama: ")))
        
        ;; Process each line of JSON
        (dolist (line (split-string output "\n" t))
          (when-let* ((json-data (ignore-errors (json-read-from-string line)))
                     (msg (alist-get 'message json-data))
                     (content (alist-get 'content msg)))
            ;; Accumulate the response
            (setq ollama-complete--current-response 
                  (concat ollama-complete--current-response content))
            ;; Update the display
            (let ((inhibit-read-only t))
              (delete-region (line-beginning-position) (point-max))
              (insert ollama-complete--current-response))
            (redisplay)))))))

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
      ;; Reset the accumulated response
      (setq-local ollama-complete--current-response "")
      (insert "\n\nOllama is thinking...\n")
      (set-process-filter proc #'ollama-complete--process-filter)
      (set-process-sentinel 
       proc
       (lambda (proc event)
         (when (string-match-p "finished" event)
           (with-current-buffer (process-buffer proc)
             (goto-char (point-max))
             (insert "\n----------------------------------------\n\n"))))))))

(provide 'ollama-complete)
