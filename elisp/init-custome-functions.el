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

;; Kill all buffers
(defun close-all-buffers ()
  "Close all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun create-temp-file (fname)
  "Create temp file, if FNAME is nil, file name will be random."
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
  "Jump to temp directory, If TEMP-INDEX is nil, directory is random."
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

(defun djcb-duplicate-line (&optional commentfirst)
  "Duplicate line; if COMMENTFIRST is non-nil, comment the original."
  (interactive)
  (beginning-of-line)
  (push-mark)
  (end-of-line)
  (let ((str (buffer-substring (region-beginning) (region-end))))
    (when commentfirst
      (comment-region (region-beginning) (region-end)))
    (insert
     (concat (if (= 0 (forward-line 1)) "" "\n") str "\n"))
    (forward-line -1)))

;; duplicate a line and comment the first
(global-set-key
 (kbd "C-c v")
 (lambda()
   (interactive)
   (djcb-duplicate-line t)))

;; duplicate a line
(global-set-key
 (kbd "C-c d")
 (lambda()
   (interactive)
   (djcb-duplicate-line nil)))


;; provide
(provide 'init-custome-functions)
;;; init-custome-functions.el ends here