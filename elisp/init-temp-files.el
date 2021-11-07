;;; Code:
(defun create-temp-file (fname)
  "Create temp file"
  (interactive "sWhat will be extension? ")
  (find-file
   (if (= (length fname) 0)
       (make-temp-name "emacs-temp")
     (concat "/tmp/" (make-temp-name "emacs-temp") "." fname)
     )
   )
  )

(defun create-temp-directory ()
  "Create temp directory."
  (interactive)
  (let ((dirs (number-sequence 0 1000)))
    (while dirs
      (let ((directory-name (format "/tmp/%d" (car dirs))))
        (if (not (file-directory-p directory-name))
            ;; (find-dired (concat "/tmp/" (car dirs))) (setq dirs (cdr dirs))
            (progn
              (make-directory directory-name)
              (find-file directory-name)
              (setq dirs nil))
          (setq dirs (cdr dirs)))
        )
      )
    )
  )

(defun jump-to-temp-directory (temp-index)
  "Jump to temp directory."
  (interactive "sWhat is temporary directory index: ")
  (if (= (length temp-index) 0)
      (create-temp-directory)
    (let ((directory-name (format "/tmp/%s" temp-index)))
      (if (not (file-directory-p directory-name))
          (progn
            (make-directory directory-name)
            (find-file directory-name))
        (find-file directory-name)
        )
      )
    )
  )

(provide 'init-temp-files)
;;; init-temp-files.el ends here
