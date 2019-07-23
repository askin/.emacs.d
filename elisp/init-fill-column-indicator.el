(require 'fill-column-indicator)

(setq-default fill-column 80)

;; Fill Column Indicator: https://github.com/alpaker/Fill-Column-Indicator
(setq fci-rule-width 5)
(setq fci-rule-color "gray20")

;; User fci-mode for defined modes
;; FIXME: fci-mode doesn't work with modes but python-mode
(mapc
 (lambda (language-mode-hook)
   (add-hook language-mode-hook 'fci-mode))
 '(python-mode-hook c-mode-hook lisp-mode-hook js-mode-hook python-mode))

(provide 'init-fill-column-indicator)
