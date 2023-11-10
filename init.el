;; emacs.el --- Emacs initialization file
;;; Commentary:
; Emacs Startup File -- initializarion for Emacs
;;; Code:


(set-language-environment 'english)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; configurations
(fset 'yes-or-no-p 'y-or-n-p)            ;; enable y/n answers to yes/no

;; Set path to dependencies
(defvar site-lisp-dir
  (expand-file-name "elisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

(setq inhibit-startup-message t          ;; don't show ...
      inhibit-startup-echo-area-message t
      custom-file                (expand-file-name "custom.el" user-emacs-directory)
      visible-bell 2
      package-archives           '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
                                   ("jcs-elpa" . "https://jcs-emacs.github.io/jcs-elpa/packages/")
                                   ("melpa"    . "https://melpa.org/packages/"))
      package-archive-priorities '(("gnu-elpa" . 0)
                                   ("jcs-elpa" . 5)
                                   ("melpa"    . 10)))

(use-package uuidgen
  :ensure t
  :init
  )

(use-package gruvbox-theme
  :ensure t
  :init
  )

(use-package theme-changer
  :ensure t
  :init

  (require 'theme-changer)
  (setq calendar-location-name "Izmir, TR"
        calendar-latitude 38.46
        calendar-longitude 27.12)

  (change-theme 'gruvbox-light-hard 'gruvbox-dark-hard)
  )


(use-package diff-hl
  :ensure t
  :init
  )


(use-package multiple-cursors
  :ensure t
  :init
  )

(use-package pyvenv
  :ensure t
  :init
  )

(use-package gist
  :ensure t
  :init

  ;; Post current buffer to gist and browse it (require gist)
  (defun gist-buffer-private-browse ()
    "Post current buffer to gist and browse it."
    (interactive)
    (let ((gist-view-gist t))
      (gist-region-private (point-min) (point-max))))

  (global-set-key (kbd "C-c b") 'gist-buffer-private-browse)
  )

(use-package ssh-config-mode
  :ensure t
  :init

  (autoload 'ssh-config-mode "ssh-config-mode" t)
  (add-to-list 'auto-mode-alist '("/\\.ssh/config\\'"     . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/sshd?_config\\'"      . ssh-config-mode))
  (add-to-list 'auto-mode-alist '("/known_hosts\\'"       . ssh-known-hosts-mode))
  (add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
  (add-hook 'ssh-config-mode-hook 'turn-on-font-lock)
  )

(use-package dash
  :ensure t
  :init
  )

(use-package all-the-icons-ivy
  :ensure t
  :init
  )

(use-package spaceline-all-the-icons
  :ensure t
  :init
  )

(use-package helm
  :ensure t
  :init
  )

(use-package flycheck
  :ensure t
  :init
  ;; Syntax Check For All Type Code
  (global-flycheck-mode)
  ;; Load all *.el files
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  )

(use-package rainbow-delimiters
  :ensure t
  :init
  )

(use-package nginx-mode
  :ensure t
  :init
  )

(use-package anzu
  :ensure t
  :init

  (global-anzu-mode t)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes
;; Don't use tabs in any text-mode
(add-hook 'text-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Keyboard Fixes

;; Disable Ctrl Z
(global-set-key (kbd "C-x C-z") nil)

;; Browse the current url
(global-set-key (kbd "C-c u") 'browse-url)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; show whitespaces at the end of the line
(setq show-trailing-whitespace t)

;; Major Mode Customization
(setq-default auto-fill-function nil)

;; Autosave & Backup & Lockfiles
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq
 backup-directory-alist (list (cons ".*" backup-dir))
 auto-save-list-file-prefix autosave-dir
 auto-save-file-name-transforms `((".*" ,autosave-dir t))
 create-lockfiles nil ;; Do not create lock file
 )

;; Remove White Spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq require-final-newline t)           ;; end files with a newline

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
(global-hl-line-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(add-hook 'python-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(global-display-fill-column-indicator-mode)

(electric-pair-mode 1)

;;; My Custom Init Files
(require 'init-cosmetic)
(require 'init-openweekly-plan)
(require 'init-custome-functions)
(require 'init-org-mode)

;; uwe web-mode for common html files

(use-package web-mode
  :ensure t
  :init
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
  :init
  (add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md.erb$" . markdown-mode))
  )

(global-diff-hl-mode)

(provide 'init)
;;; init.el ends here
