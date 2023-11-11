;;; package -- summary
;;; Commentary:
;;; Code:

;; format branch name
(defadvice vc-mode-line (after strip-backend () activate)
  (when (stringp vc-mode)
    (let ((noback (replace-regexp-in-string
                   (format "^ %s" (vc-backend buffer-file-name))
                   " " vc-mode)))
      (setq vc-mode noback))))

;; use setq-default to set it for /all/ modes
(setq-default
 mode-line-format
 (list
  ;; the buffer name; the file name as a tool tip
  '(:eval (propertize "%b" 'face 'font-lock-keyword-face
                      'help-echo (buffer-file-name)))

  '(:propertize (vc-mode vc-mode) face (:weight bold))

  " "

  ;; line and column
  "(" ;; '%02' to set to 2 chars at least; prevents flickering
  (propertize "%02l" 'face 'font-lock-type-face) ","
  (propertize "%02c" 'face 'font-lock-type-face)
  ") "

  ;; relative position, size of file
  "["
  (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
  "/"
  (propertize "%I" 'face 'font-lock-constant-face) ;; size
  "] "

  ;; the current major mode for the buffer.
  "["

  '(:eval (propertize "%m" 'face 'font-lock-string-face
                      'help-echo buffer-file-coding-system))
  "] "


  "[" ;; insert vs overwrite mode, input-method in a tooltip
  '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                      'face 'font-lock-preprocessor-face
                      'help-echo (concat "Buffer is in "
                                         (if overwrite-mode "overwrite" "insert") " mode")))

  ;; was this buffer modified since the last save?
  '(:eval (when (buffer-modified-p)
            (concat ","  (propertize "Mod"
                                     'face 'font-lock-warning-face
                                     'help-echo "Buffer has been modified"))))

  ;; is this buffer read-only?
  '(:eval (when buffer-read-only
            (concat ","  (propertize "RO"
                                     'face 'font-lock-type-face
                                     'help-echo "Buffer is read-only"))))
  "] "

  ;; add the time, with the date and the emacs uptime in the tooltip
  '(:eval (propertize (format-time-string "%H:%M")
                      'help-echo
                      (concat (format-time-string "%c; ")
                              (emacs-uptime "Uptime:%hh"))))
  " --"
  ;; i don't want to see minor-modes; but if you want, uncomment this:
  minor-mode-alist  ;; list of minor modes
  "%-" ;; fill with '-'
  )
 )

;; Title Format
(setq-default
 frame-title-format
 '(:eval
   (format "%s@%s:%s%s"
           (or (file-remote-p default-directory 'user) user-login-name)
           (or (file-remote-p default-directory 'host) system-name)
           (file-name-directory (or (buffer-file-name) default-directory))
           (file-name-nondirectory (or (buffer-file-name) default-directory)))))

;; Show line numbers
(global-display-line-numbers-mode)

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

(global-diff-hl-mode)

(provide 'init-cosmetic)
;;; init-cosmetic.el ends here
