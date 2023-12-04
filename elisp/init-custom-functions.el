;;; init-flatten-log-file.el --- Flatten log files
;;; Commentary:
;; Flatten log files

;;; Code:
(defun flatten-log-file ()
  "Flatten log files."
  (interactive)
  (mapcar
   (lambda (it)
     (goto-char (point-min))
     (while (search-forward (car it) nil t)
       (replace-match (cadr it))))
   '(("\\n" "\n") ("\\t" "\t"))))

;; Kill all buffers
(defun close-all-buffers ()
  "Close all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))


(defgroup custom-functions nil
  "Custom functions default group."
  :group 'text
  :prefix "custom-functions-"
  :link '(url-link "https://github.com/askin/.emacs.d"))

(defcustom custom-functions-temp-file-prefix "emacs-temp-"
  "Default prefix for create-temp-file."
  :group 'custom-functions
  :type 'string)

(defcustom custom-functions-temp-file-default-suffix ".txt"
  "Default suffix for create-temp-file."
  :group 'custom-functions
  :type 'string)

(defun create-temp-file (fname)
  "Create temp file, if FNAME is nil, file name will be random."
  (interactive "sWhat will be extension? ")
  (let ((suffix (if (= (length fname) 0)
                    custom-functions-temp-file-default-suffix
                  (concat "." fname))))
    (find-file (make-temp-file custom-functions-temp-file-prefix nil suffix nil))))


(defun first (check-function listparam)
  "Return fist matching item.  CHECK-FUNCTION for checking, LISTPARAM is for data."
  (if (funcall check-function (car listparam))
      (car listparam)
    (first check-function (cdr listparam))
  ))

(defun create-temp-directory ()
  "Create temp directory."
  (interactive)
  (let ((directory-name
         (first
          (lambda (directory) (not (file-directory-p directory)))
          (mapcar (lambda (item) (format "/tmp/%d" item)) (number-sequence 0 1000)))))
    (make-directory directory-name)
    (find-file directory-name)))

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
(provide 'init-custom-functions)
;;; init-custom-functions.el ends here
