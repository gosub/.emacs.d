(defun gg/search-on-youtube ()
  "search region text on youtube"
  (interactive)
  (browse-url (concat "https://www.youtube.com/results?search_query="
		      (buffer-substring (mark) (point)))))


(defun gg/search-on-google ()
  "search region text on google"
  (interactive)
  (browse-url (concat "https://www.google.com/search?q="
		      (buffer-substring (mark) (point)))))


(defun gg/search-on-hackernews ()
  "search region text on hacker news"
  (interactive)
  (browse-url (concat "https://hn.algolia.com/?q="
		      (buffer-substring (mark) (point)))))


(provide 'gg-websearch)
