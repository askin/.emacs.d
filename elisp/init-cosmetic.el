;;; package -- summary
;;; Commentary:
;;; Code:
;; Disable Emacs splash screen
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(progn
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-scroll-bar-mode nil)
  (menu-bar-mode 0))

(require 'theme-changer)
(change-theme 'gruvbox-light-hard 'gruvbox-dark-hard)

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

(set (make-local-variable lisp-indent-function)
     'common-lisp-indent-function)

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
(if (version<= "26.0.50" emacs-version )
    (global-display-line-numbers-mode)
  (global-linum-mode 0))

(column-number-mode t)                   ;; show column numbers
(size-indication-mode t)                 ;; show file size (emacs 22+)

;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

;; Set Font and Size
(set-face-attribute 'default nil :font "Fira Code" :height 105)

;; Emacs Window Geometry
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 145))

(provide 'init-cosmetic)
;;; init-cosmetic.el ends here
