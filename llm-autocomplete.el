;;; llm-autocomplete.el --- Local LLM-powered autocomplete for Emacs

(require 'json)
(require 'request)
(require 'cl-lib)

(defgroup llm-autocomplete nil
  "Local LLM-powered autocompletion."
  :group 'completion)

(defcustom llm-autocomplete-endpoint "http://localhost:11434/api/generate"
  "Endpoint URL for the local LLM server."
  :type 'string
  :group 'llm-autocomplete)

(defcustom llm-autocomplete-model "llama3.2"
  "Model to use for completion."
  :type 'string
  :group 'llm-autocomplete)

(defcustom llm-autocomplete-context-length 1000
  "Number of characters before point to send as context."
  :type 'integer
  :group 'llm-autocomplete)

;; Store ongoing and cached completions
(defvar-local llm-autocomplete--ongoing-request nil
  "Ongoing request identifier for LLM completions.")
(defvar-local llm-autocomplete--cached-completions nil
  "Cached completions for the current context.")

(defun llm-autocomplete--get-context ()
  "Get context before point for LLM completion."
  (let* ((pos (point))
         (start (max (- pos llm-autocomplete-context-length) (point-min))))
    (message "Position: %s, Start: %s" pos start)
    (let ((context (buffer-substring-no-properties start pos)))
      (message "Context: %s" context)
      context)))

(defun llm-autocomplete--query-llm (prompt callback)
  "Send PROMPT to the LLM server and call CALLBACK with the result."
  (let ((request-id (make-symbol "llm-request")))
    (setq llm-autocomplete--ongoing-request request-id)
    (request
     llm-autocomplete-endpoint
     :type "POST"
     :data (json-encode `(("model" . ,llm-autocomplete-model)
                          ("prompt" . ,prompt)))
     :headers '(("Content-Type" . "application/json"))
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (when (eq llm-autocomplete--ongoing-request request-id)
                   (let ((reply (gethash "response" data)))
                     (when reply
                       (setq llm-autocomplete--cached-completions
                             (split-string reply "\n" t))
                       (message "Received completions: %s" 
                                llm-autocomplete--cached-completions)
                       (funcall callback llm-autocomplete--cached-completions)))))))
     :error (cl-function
             (lambda (&key error-thrown &allow-other-keys)
               (message "LLM request error: %S" error-thrown)))))

(defun llm-autocomplete-fetch-dynamic (callback)
  "Fetch completions dynamically for completion-at-point."
  (if llm-autocomplete--cached-completions
      (funcall callback llm-autocomplete--cached-completions)
    (llm-autocomplete--query-llm 
     (llm-autocomplete--get-context)
     (lambda (completions)
       (setq llm-autocomplete--cached-completions completions)
       (funcall callback completions)))))


(defun llm-autocomplete-complete-at-point ()
  "Provide completions at point using LLM."
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (start (or (car bounds) (point)))
         (end (or (cdr bounds) (point)))
         (prefix (buffer-substring-no-properties start end)))

    ;; If cached completions exist, use them immediately
    (if llm-autocomplete--cached-completions
        (progn
          (message "Using cached completions: %s" llm-autocomplete--cached-completions)
          (list start end (completion-table-dynamic
                           (lambda (_string) llm-autocomplete--cached-completions))))

      ;; Otherwise, initiate a fresh LLM request asynchronously
      (progn
        (message "Requesting LLM completions...")
        (setq llm-autocomplete--cached-completions nil)
        (llm-autocomplete--query-llm
         (llm-autocomplete--get-context)
         (lambda (completions)
           (setq llm-autocomplete--cached-completions completions)
           (message "LLM returned completions: %s" completions)
           ;; Trigger completion again after receiving the results
           (completion-in-region start end
                                 (completion-table-dynamic
                                  (lambda (_string) llm-autocomplete--cached-completions))))))
    
    ;; Return a valid completion function immediately (even before LLM returns)
    (list start end (completion-table-dynamic
                     (lambda (_string) llm-autocomplete--cached-completions))))))

;;;###autoload
(define-minor-mode llm-autocomplete-mode
  "Toggle LLM-powered autocompletion."
  :lighter " LLM"
  :global nil
  (if llm-autocomplete-mode
      (add-hook 'completion-at-point-functions
                #'llm-autocomplete-complete-at-point)
    (remove-hook 'completion-at-point-functions
                 #'llm-autocomplete-complete-at-point)))

(defun llm-test-query ()
  "Test sending a query to the LLM server."
  (request
   "http://localhost:11434/api/generate"
   :type "POST"
   :data (json-encode `((model . "llama3.2")
                        (prompt . "test context")))
   :headers '(("Content-Type" . "application/json"))
   :parser 'buffer-string
   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (message "Success! Response: %s" data)))
   :error (cl-function
           (lambda (&key error-thrown &allow-other-keys)
             (message "Error! %S" error-thrown)))))


(provide 'llm-autocomplete)
;;; llm-autocomplete.el ends here
