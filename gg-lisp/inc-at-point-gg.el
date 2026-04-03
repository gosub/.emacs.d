;;; inc-at-point-gg.el --- Increment/decrement number at point -*- lexical-binding: t; -*-

(defun inc-at-point-gg ()
  "Increment number at point, partially simulating C-a in vim."
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (unless (looking-at "[0123456789]+")
      (error "No number at point"))
    (replace-match (number-to-string
                    (1+ (string-to-number (match-string 0)))))))

(defun dec-at-point-gg ()
  "Decrement number at point, partially simulating C-x in vim."
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (unless (looking-at "[0123456789]+")
      (error "No number at point"))
    (replace-match (number-to-string
                    (1- (string-to-number (match-string 0)))))))

(provide 'inc-at-point-gg)
;;; inc-at-point-gg.el ends here
