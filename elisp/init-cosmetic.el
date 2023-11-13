;;; package -- summary
;;; Commentary:
;;; Code:

;; Title Format
(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (file-name-directory (or (buffer-file-name) default-directory))
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))

;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

;; lambda to λ
(defun my-pretty-lambda ()
  "Make some word or string show as pretty Unicode symbols."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))

(add-hook 'scheme-mode-hook 'my-pretty-lambda)
(global-prettify-symbols-mode 1)

(setq initial-scratch-message
      (with-temp-buffer
        (insert-file-contents (expand-file-name "ascii.txt" user-emacs-directory))
        (buffer-string)))

(provide 'init-cosmetic)
;;; init-cosmetic.el ends here
