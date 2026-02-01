;;; gg-org-yt.el --- YouTube to org-mode utilities -*- lexical-binding: t; -*-

(defun gg/yt-playlist-to-org (playlist-url)
  "Turn a YouTube playlist link into an org section, where each video is a subsection."
  (interactive "sPlaylist url or id: ")
  (let* ((header-lines
          (process-lines
           "yt-dlp" "--ignore-errors" "--get-filename"
           "--output"
           "* [/] [[https://www.youtube.com/playlist?list=%(playlist_id)s][%(uploader)s - %(playlist_title)s]]"
           "--playlist-end" "1" playlist-url))
         (playlist-header (mapconcat #'identity header-lines "\n"))
         (entries-lines
          (process-lines
           "yt-dlp" "--ignore-errors" "--get-filename"
           "--output"
           "- [ ] [[https://www.youtube.com/watch?v=%(id)s][%(title)s]]"
           playlist-url))
         (playlist-entries (mapconcat #'identity entries-lines "\n")))
    (insert (concat "\n" playlist-header "\n" playlist-entries "\n"))))

(provide 'gg-org-yt)
;;; gg-org-yt.el ends here
