;;; gg-websearch.el --- Search the web from Emacs -*- lexical-binding: t; -*-

;;; Code:

(require 'url-util)

(defun gg-search-on-youtube (query)
  "Search QUERY on YouTube."
  (browse-url (concat "https://www.youtube.com/results?search_query="
                      (url-hexify-string query))))

(defun gg-search-on-google (query)
  "Search QUERY on Google."
  (browse-url (concat "https://www.google.com/search?q="
                      (url-hexify-string query))))

(defun gg-search-on-hackernews (query)
  "Search QUERY on Hacker News."
  (browse-url (concat "https://hn.algolia.com/?q="
                      (url-hexify-string query))))

(defun gg-websearch-dwim ()
  "Search the web with selected engine.
If region is active, search for it; otherwise prompt for query."
  (interactive)
  (let* ((engines '(("google" . gg-search-on-google)
                    ("youtube" . gg-search-on-youtube)
                    ("hn" . gg-search-on-hackernews)))
         (engine (completing-read "Engine: " engines nil t))
         (query (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (read-string "Search: ")))
         (fn (cdr (assoc engine engines))))
    (funcall fn query)))

(provide 'gg-websearch)
;;; gg-websearch.el ends here
