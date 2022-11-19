(defun djcb-duplicate-line (&optional commentfirst)
  "comment line at point; if COMMENTFIRST is non-nil, comment the original"
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
(global-set-key (kbd "C-c d") "\C-a\C- \C-n\M-w\C-y")

;; provide
(provide 'init-duplicate)
