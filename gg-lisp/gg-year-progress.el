;;; gg-year-progress.el --- Visual year progress display -*- lexical-binding: t; -*-

(defface year-progress-dark-dot
  '((t (:foreground "#666666" :weight bold)))
  "Face for days that have already passed in the year."
  :group 'year-progress)

(defface year-progress-light-dot
  '((t (:foreground "#cccccc" :weight bold)))
  "Face for days that have not yet passed in the year."
  :group 'year-progress)

(defun display-year-progress ()
  "Display a visual representation of passed and remaining days in the current year.
Uses dark dots for passed days and light dots for remaining days."
  (interactive)
  (let* ((current-date (decode-time (current-time)))
         (current-year (nth 5 current-date))
         (is-leap-year (date-leap-year-p current-year))
         (days-in-year (if is-leap-year 366 365))
         (current-day-of-year (1+ (time-to-day-in-year (current-time))))
         (buffer (get-buffer-create "*Year Progress*"))
         (dot "●"))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Progress for %d (%d days total):\n\n"
                      current-year days-in-year))
      (dotimes (day days-in-year)
        (let ((face (if (<= (1+ day) current-day-of-year)
                       'year-progress-dark-dot
                       'year-progress-light-dot)))
          (insert (propertize dot 'face face))
          (insert " ")
          (when (and (= (mod day 30) 29) (< day (- days-in-year 1)))
            (insert "\n"))))
      (insert "\n\n")
      (insert (format "Days passed: %d\n" current-day-of-year))
      (insert (format "Days remaining: %d\n" (- days-in-year current-day-of-year)))
      (insert (format "Progress: %.1f%%\n"
                      (* 100.0 (/ current-day-of-year (float days-in-year)))))
      (insert (format "Leap year: %s\n" (if is-leap-year "Yes" "No")))
      (goto-char (point-min))
      (display-buffer buffer))))

(provide 'gg-year-progress)
;;; gg-year-progress.el ends here
