;; Org-mode
;; (require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


;; Automatically change to DONE when all children are done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "IN_PROGRESS(i@/!)" "IZLE(z)" "|" "DONE(d!)" "CANCELED(c@)" "DELEGATE(f@)" "IZLENDI(n)")
        (sequence "READ(r) POSTPONED(p)" "|" "READED(d!)")
        (sequence "WATCH(w)" "|" "WATCHED(d!)")))

(provide 'init-org-mode)
