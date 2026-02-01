;;; gg-inc-at-point.el --- Increment number at point -*- lexical-binding: t; -*-

(defun gg/increment-number-at-point ()
  "Increment number at point, partially simulating C-a in vim."
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
	(error "No number at point"))
    (replace-match (number-to-string
                    (1+ (string-to-number (match-string 0)))))))

(provide 'gg-inc-at-point)
;;; gg-inc-at-point.el ends here
