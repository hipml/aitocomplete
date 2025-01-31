(require 'company)
(require 'llm-autocomplete)

(defun company-llm--candidates (prefix)
  "Get completion candidates from LLM for PREFIX."
  (when (and (not llm-autocomplete--ongoing-request)
             (> (length prefix) 0))
    (llm-autocomplete--query-llm
     (llm-autocomplete--get-context)
     (lambda (completions)
       (setq llm-autocomplete--cached-completions completions)))
    llm-autocomplete--cached-completions))

(defun company-llm (command &optional arg &rest _ignored)
  "Company backend for LLM-powered completion."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-llm))
    (prefix (and (not (company-in-string-or-comment))
                 (or (company-grab-symbol) 'stop)))
    (candidates (company-llm--candidates arg))
    (sorted t)))

;;;###autoload
(defun company-llm-setup ()
  "Set up company-llm."
  (interactive)
  (add-to-list 'company-backends 'company-llm))

(provide 'company-llm)
