;;; roma-random-place.el --- Random Rome coordinates -*- lexical-binding: t; -*-

(defun roma-random-place ()
  "Generate a Google Maps URL for a random location in Rome.
Opens the URL in the default browser."
  (interactive)
  (let* ((lat (+ 41.792763 (* (/ (random 1000000) 1000000.0) (- 41.987279 41.792763))))
         (lon (+ 12.504539 (* (/ (random 1000000) 1000000.0) (- 12.617532 12.504539))))
         (url (format "google.it/maps/place//@%.6f,%.6f,17z" lat lon)))
    (browse-url url)))

(provide 'roma-random-place)
;;; roma-random-place.el ends here
