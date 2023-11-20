;; emacs.el --- Emacs initialization file
;;; Commentary:
; Emacs Startup File -- initializarion for Emacs
;;; Code:


(progn
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (set-scroll-bar-mode nil)
  (menu-bar-mode 0))

;; configurations
(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no

;; Set path to dependencies
(defvar site-lisp-dir
  (expand-file-name "elisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

(use-package emacs
  :init
  :config
  (setq show-trailing-whitespace t ;; show whitespaces at the end of the line
	require-final-newline t
	inhibit-startup-message t          ;; don't show ...
	inhibit-startup-echo-area-message t
	custom-file                (expand-file-name "custom.el" user-emacs-directory)
	visible-bell 2
	package-archives           '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                                     ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                                     ("melpa"    . "https://melpa.org/packages/"))
	package-archive-priorities '(("gnu-elpa" . 0)
                                     ("jcs-elpa" . 5)
                                     ("melpa"    . 10))
	)
  (progn                                              ;; Set Font and Size
    (defvar my-font "Fira Code")
    (set-face-attribute 'default nil :font my-font :height 95)
    (add-to-list 'default-frame-alist '(height . 40)) ;; Emacs Window Geometry
    (add-to-list 'default-frame-alist '(width . 145)) ;; Emacs Window Geometry
    (defun set-face-attribute-for-28-inch ()
      (interactive)
      (set-face-attribute 'default nil :font my-font :height 95))
    (defun set-face-attribute-for-14-inch ()
      (interactive)
      (set-face-attribute 'default nil :font my-font :height 105)))
  (progn ;; Autosave & Backup & Lockfiles
    (defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
    (defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
    (setq backup-directory-alist (list (cons ".*" backup-dir))
	  auto-save-list-file-prefix autosave-dir
	  auto-save-file-name-transforms `((".*" ,autosave-dir t))
	  create-lockfiles nil))
  (global-hl-line-mode 1)
  (global-display-line-numbers-mode) ;; Show line numbers
  (global-display-fill-column-indicator-mode)
  (electric-pair-mode 1)
  (global-diff-hl-mode)
  :bind (("C-x C-z" . nil) ;; Disable Ctrl Z
	 ("C-c u" . 'browse-url))
  :hook ((before-save . delete-trailing-whitespace)
	 (prog-mode   . rainbow-delimiters-mode)))

(use-package uuidgen :ensure t)
(use-package gruvbox-theme :ensure t)
(use-package diff-hl :ensure t)
(use-package multiple-cursors :ensure t)
(use-package pyvenv :ensure t)
(use-package dash :ensure t)
(use-package all-the-icons-ivy :ensure t)
(use-package spaceline-all-the-icons :ensure t)
(use-package helm :ensure t)
(use-package rainbow-delimiters :ensure t)
(use-package nginx-mode :ensure t)
(use-package ssh-config-mode :ensure t)

(use-package theme-changer
  :ensure t
  :config
  (setq calendar-location-name "Izmir, TR"
        calendar-latitude 38.46
        calendar-longitude 27.12)
  (change-theme 'gruvbox-light-hard 'gruvbox-dark-hard)
  )

(use-package gist
  :ensure t
  :preface
  (defun gist-buffer-private-browse ()
    "Post current buffer to gist and browse it."
    (interactive)
    (let ((gist-view-gist t))
      (gist-region-private (point-min) (point-max))))
  :config
  :bind (("C-c b" . gist-buffer-private-browse))
  )

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode) ;; Syntax Check For All Type Code
  (setq-default flycheck-emacs-lisp-load-path 'inherit)   ;; Load all *.el files
  )

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode t)
  )

(use-package web-mode
  :config
  (setq
   web-mode-enable-auto-indentation nil
   web-mode-markup-indent-offset 4
   web-mode-script-padding 0
   web-mode-code-indent-offset 4
   web-mode-style-padding 0
   web-mode-css-indent-offset 4)

  (add-to-list 'auto-mode-alist '("\\.tpl$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html$'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.inc$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mjml$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml$" . web-mode))
  )

(use-package markdown-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md.erb$" . markdown-mode))
  :custom-face (markdown-code-face ((t (nil))))
  )

(use-package markdown-preview-mode
  :ensure t
  :config
  (setq markdown-command "~/bin/markdown"
	markdown-preview-stylesheets (list "https://cdn.askin.ws/emacs/markdown-preview-mode-do-style.css"))
  )

;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(when (fboundp 'show-paren-mode)
  (show-paren-mode t)
  (defvar show-paren-style 'parenthesis))

;; Overrride the default function param
(defun emacs-session-filename (SESSION-ID)
  "Overrride the default function param: SESSION-ID ...."
  (concat "~/.emacs.d/cache/session." SESSION-ID)
  )

;; bookmarks
(defvar bookmark-default-file "~/.emacs.d/data/bookmarks")
;; autosave each change
(defvar bookmark-save-flag 1)

;; savehist: save my search entries
(defvar savehist-additional-variables '(search ring regexp-search-ring))
;; save every minute (default: 5 min)
(defvar savehist-autosave-interval 60)
;; keep my home clean
(defvar savehist-file "~/.emacs.d/cache/savehist")
(savehist-mode t)                      ;; do customization before activation

;; activate uppercase - lowercase functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;; My Custom Init Files
(require 'init-cosmetic)
(require 'init-mode-line)
(require 'init-openweekly-plan)
(require 'init-custome-functions)
(require 'init-org-mode)

(provide 'init)
;;; init.el ends here
