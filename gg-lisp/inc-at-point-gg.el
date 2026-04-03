;;; inc-at-point-gg.el --- Increment/decrement number at point -*- lexical-binding: t; -*-

(defun inc-at-point-gg (count)
  "Increment number at point by COUNT, partially simulating C-a in vim."
  (interactive "p")
  (save-excursion
    (skip-chars-backward "0123456789")
    (unless (looking-at "[0123456789]+")
      (error "No number at point"))
    (replace-match (number-to-string
                    (+ (string-to-number (match-string 0)) count)))))

(defun dec-at-point-gg (count)
  "Decrement number at point by COUNT, partially simulating C-x in vim."
  (interactive "p")
  (save-excursion
    (skip-chars-backward "0123456789")
    (unless (looking-at "[0123456789]+")
      (error "No number at point"))
    (replace-match (number-to-string
                    (- (string-to-number (match-string 0)) count)))))

(provide 'inc-at-point-gg)
;;; inc-at-point-gg.el ends here
