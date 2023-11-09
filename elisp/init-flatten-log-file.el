;;; init-flatten-log-file.el --- Flatten log files
;;; Commentary:
;; Flatten log files

;;; Code:

(defun flatten-log-file ()
  "Flatten log files."
  (interactive)
  (setq charlist '(("\\n" "\n") ("\\t" "\t")))
  (while charlist
    (setq item (car charlist))
    (goto-char (point-min))
    (while (search-forward (car item) nil t)
      (replace-match (car (cdr item))))
    (setq charlist (cdr charlist))))


;; provide
(provide 'init-flatten-log-file)
;;; init-flatten-log-file.el ends here
