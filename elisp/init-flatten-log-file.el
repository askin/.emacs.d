;;; init-flatten-log-file.el --- Flatten log files
;;; Commentary:
;; Flatten log files

;;; Code:
(require 'dash)

(defun flatten-log-file ()
  "Flatten log files."
  (interactive)
  (let ((charlist '(("\\n" "\n") ("\\t" "\t"))))
    (--each charlist
      (goto-char (point-min))
      (while (search-forward (car it) nil t)
        (replace-match (car (cdr it)))))))


;; provide
(provide 'init-flatten-log-file)
;;; init-flatten-log-file.el ends here
