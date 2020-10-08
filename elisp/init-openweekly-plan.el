;;; package -- summary
;;; Commentary:
;;; Code:
;; Open Weekly Agenda

(require 'calendar)

(defun open-weekly-plan ()
  "Opens weekly plan."
  (interactive)
  (find-file
   (let ((date (calendar-current-date)))
     (cl-destructuring-bind (month day year)
         (calendar-gregorian-from-absolute
          (1+ (- (calendar-absolute-from-gregorian date)
                 (calendar-day-of-week date))))
       (format "~/projects/todos/weekly-personel/%04d-%02d-%02d.org" year month day)))))

(provide 'init-openweekly-plan)
;;; init-openweekly-plan.el ends here
