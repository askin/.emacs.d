;; akor mode
;; notes
;; quick hack of syntax highlighting for asterisk dialplan el file originally posted by Michael Taht
;; this is a crude enhancement but better than pages of text all the same colour ... appeal to experts in emacs/lisp/regex syntax to please improve
(setq max-specpdl-size 6000)
(define-generic-mode 'akor-mode
  ;; comments
  '()
  ;; keywords
  ;; these are all case sensitive ... asterisk isn't would be nice to fix
  '()
  ;; colours for the important bits ... need to find more faces and have
  ;; highlights within highlights ... ie ${} within $[ ]
  '(
    ("^[A-Za-z]+:" . 'font-lock-function-name-face)
    ("^.* - .*$" . 'font-lock-function-name-face)
    ("^.* - .*$" . 'font-lock-keyword-face)
    ("^[A-G][#|b]?[m|M]?[0-9]?$" . 'font-lock-keyword-face)
    (" [A-G][#|b]?[m|M]?[0-9]?$" . 'font-lock-keyword-face)
    ("^[A-G][#|b]?[m|M]?[0-9]? " . 'font-lock-keyword-face)
    (" [A-G][#|b]?[m|M]?[0-9]? " . 'font-lock-keyword-face)
    (" [A-G][#|b]?[m|M]?[0-9]-+? " . 'font-lock-keyword-face)
    ("" . 'font-lock-constant-face )
    ("" . 'font-lock-variable-name-face)
    )
  ;; files
  ;; left ... was only interested in improving readability of dialplans
  ;; god I hate writing dialplans ... but things are way better since gosubs appeared
  '("^Ritm:" "^Intro:")
  nil
  "Mode for editing asterisk config files")
