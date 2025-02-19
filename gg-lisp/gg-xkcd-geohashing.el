(defun gg/calculate-xkcd-geohashing-coords (latitude longitude)
  "Calculate the coordinates of today's XKCD geohashing game.
LATITUDE and LONGITUDE must be integers.
The Dow Jones opening value is downloaded from URL `http://geo.crox.net/djia/'.
The algorithm was invented for URL `https://xkcd.com/426/'.
See URL `https://geohashing.site/' for additional info."
  (let*
      ((djia-url (concat "http://geo.crox.net/djia/"
			 (format-time-string "%Y/%m/%d")))
       (djia (with-temp-buffer
	       (url-insert-file-contents djia-url)
	       (buffer-string)))
       (daily-pre-hash (concat
			(format-time-string "%Y-%m-%d")
			"-" djia))
       (daily-hash (md5 daily-pre-hash))
       (lat-frac-hex (substring daily-hash 0 16))
       (lon-frac-hex (substring daily-hash 16))
       (lat-frac (calc-eval (concat "16#0." lat-frac-hex)))
       (lon-frac (calc-eval (concat "16#0." lon-frac-hex)))
       (lat (+ latitude (string-to-number lat-frac)))
       (lon (+ longitude (string-to-number lon-frac))))
    (list lat lon)))


(defun gg/xkcd-geohashing-coords ()
  "Print the coordinates of today's XKCD geohashing game."
  (interactive)
  (let* ((lat (truncate (if (boundp 'calendar-latitude)
			    calendar-latitude
			  (read-number "Latitude: " 41))))
	 (lon (truncate (if (boundp 'calendar-longitude)
			    calendar-longitude
			  (read-number "Latitude: " 12))))
	 (coords (gg/calculate-xkcd-geohashing-coords lat lon)))
    (message
     (format "LAT %.6f, LON %.6f" (car coords) (cadr coords)))))


(defun gg/browse-xkcd-geohashing-map ()
  "Open a web map URL of today's XKCD geohashing game coordinates."
  (interactive)
  (let* ((lat (truncate (if (boundp 'calendar-latitude)
			    calendar-latitude
			  (read-number "Latitude: " 41))))
	 (lon (truncate (if (boundp 'calendar-longitude)
			    calendar-longitude
			  (read-number "Latitude: " 12))))
	 (coords (gg/calculate-xkcd-geohashing-coords lat lon))
	 (url-fmt "https://www.openstreetmap.org/?mlat=%.6f&mlon=%.6f&zoom=10")
	 (url (format url-fmt (car coords) (cadr coords))))
    (browse-url url)))

(provide 'gg-xkcd-geohashing)
