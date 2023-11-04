(defun flatten-log-file (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original"
  (interactive)
  (beginning-of-buffer)
  (replace-string "\\n" "\n")
  (beginning-of-buffer)
  (replace-string "\\t" "\t")
  )

;; provide
(provide 'init-flatten-log-file)
