;;; emacs.el --- Emacs initialization file
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
(setq initial-scratch-message
      ";; scratch buffer created -- happy hacking\n")

(setq inhibit-startup-message t          ;; don't show ...
      inhibit-startup-echo-area-message t
      visible-bell 2)   ;; ... startup messages

;; Set path to dependencies
(defvar site-lisp-dir
  (expand-file-name "elisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

(require 'package)
(setq package-check-signature nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))
  )

(package-initialize)

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa-stable")
  (package-refresh-contents))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file (expand-file-name "data/bookmarks" user-emacs-directory))
 '(package-selected-packages
   (quote
    (typescript-mode go-mode pyvenv ac-emacs-eclim lsp-mode projectile gist ssh-config-mode dash gruvbox-theme all-the-icons-ivy use-package spaceline-all-the-icons markdown-preview-mode dockerfile-mode company-php json-mode magit markdown-mode helm flycheck web-mode yaml-mode rainbow-delimiters vue-mode php-mode elpy nginx-mode anzu))))

(setq package-pinned-packages '())

(package-install 'dash)
(require 'dash)

(defun packages-install (packages)
  "Install all given package in list PACKAGES ."
  (--each packages
    (when (not (package-installed-p it))
      (package-install it)))
  (delete-other-windows))

(packages-install package-selected-packages)

(require 'init-cosmetic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes
;; Don't use tabs in any text-mode
(add-hook 'text-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

;; Indentation
;; (setq standart-indent 4)
(setq-default indent-tabs-mode t)

;; C indent with 4 space
;; (setq c-default-style "bsd"
;;      c-basic-offset 4)

(setq-default c-basic-offset 4)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Keyboard Fixes
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)

;; Disable Ctrl Z
(global-set-key (kbd "C-x C-z") nil)

;; Auto Complete
(global-set-key (kbd "<f5>") 'hippie-expand)

;; Browse selected url
(global-set-key (kbd "C-c f") 'browse-url)  ;; Firefox
;; (global-set-key (kbd "C-c s") 'speedbar)
(global-set-key (kbd "C-x g") 'goto-line)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; show whitespaces at the end of the line
(setq show-trailing-whitespace t)

;; Major Mode Customization
(setq-default auto-fill-function nil)
(setq initial-scratch-message nil)

;; Autosave & Backup & Lockfiles
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
;; Do not create lock file
(setq create-lockfiles nil)

;; Remove White Spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(remove-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; we need a bit more funky pattern, as tramp will start $SHELL
;; (sudo -s), ie., zsh for root user
(setq shell-prompt-pattern "^[^a-zA-Z].*[#$%>] *")

;; turkish asciify && deasciify
;; -- (load-file "~/.elisp/turkish.el")

;; ssh-config-mode
(autoload 'ssh-config-mode "ssh-config-mode" t)
(add-to-list 'auto-mode-alist '("/\\.ssh/config\\'"     . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/sshd?_config\\'"      . ssh-config-mode))
(add-to-list 'auto-mode-alist '("/known_hosts\\'"       . ssh-known-hosts-mode))
(add-to-list 'auto-mode-alist '("/authorized_keys2?\\'" . ssh-authorized-keys-mode))
(add-hook 'ssh-config-mode-hook 'turn-on-font-lock)

;; activate uppercase - lowercase functions
(global-hl-line-mode 1)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Python lambda-mode
(require 'lambda-mode)
(add-hook 'python-mode-hook #'lambda-mode 1)
(setq lambda-symbol (string (make-char 'greek-iso8859-7 107)))

(add-hook 'python-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(if (version< emacs-version "27.0.90")
    (add-hook 'prog-mode-hook #'fci-mode)
  (global-display-fill-column-indicator-mode))

;; Python pep8 hooks
(add-hook 'python-mode-hook
          (lambda ()
            (local-set-key  (kbd "C-c p") 'pep8)))

;; Php Modes
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Kill all buffers
(defun close-all-buffers ()
  "Close all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; open .md and .markdown files with markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.md.erb$" . markdown-mode))

;; Post current buffer to gist and browse it (require gist)
(defun gist-buffer-private-browse ()
  "Post current buffer to gist and browse it."
  (interactive)
  (let ((gist-view-gist t))
    (gist-region-private (point-min) (point-max))))

(global-set-key (kbd "C-c b") 'gist-buffer-private-browse)

;; Browse the current url
(global-set-key (kbd "C-c u") 'browse-url)

(require 'init-org-mode)

(setq initial-scratch-message
      (with-temp-buffer
        (insert-file-contents (expand-file-name "ascii.txt" user-emacs-directory))
        (buffer-string)))

(electric-pair-mode 1)

;; init duplicate lines
(require 'init-duplicate)
(require 'init-openweekly-plan)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(elpy-enable)

;; uwe web-mode for common html files
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;; Syntax Check For All Type Code
(global-flycheck-mode)
;; Load all *.el files
(setq-default flycheck-emacs-lisp-load-path 'inherit)
;;; emacs.el ends here

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'chocolate t)
;; (load-theme 'zenburn t)
;; (load-theme 'anti-zenburn t)
;; (load-theme 'labburn t)
;; (load-theme 'gruvbox t)
;; (load-theme 'Deviant t)
;; (load-theme 'dracula t)
;; (load-theme 'solarized-light t)
;; (load-theme 'solarized-dark t)

(global-anzu-mode +1)

(defun insert-current-date () (interactive)
       (insert (shell-command-to-string "echo -n $(date +\"%Y-%m-%d %H:%M:%S\")")))

(setq js-indent-level 2)

(provide 'init)
;;; init.el ends here

;; 'init-openweekly-plan
