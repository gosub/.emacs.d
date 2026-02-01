;;; gg-org-yt.el --- YouTube to org-mode utilities -*- lexical-binding: t; -*-

(defun gg/yt-playlist-to-org (playlist-url)
  "Turn a YouTube playlist link into an org section, where each video is a subsection."
  (interactive "sPlaylist url or id: ")
  (let*
      ((header-cmd (concat "yt-dlp"
			   " --ignore-errors --get-filename "
			   " --output '* [/] [[https://www.youtube.com/playlist?list=%(playlist_id)s][%(uploader)s - %(playlist_title)s]]' "
			   " --playlist-end 1 "))
       (playlist-header (shell-command-to-string (concat header-cmd playlist-url)))
       (entries-cmd (concat "yt-dlp"
			    " --ignore-errors --get-filename "
			    " --output '- [ ] [[https://www.youtube.com/watch?v=%(id)s][%(title)s]]' "))
       (playlist-entries (shell-command-to-string (concat entries-cmd playlist-url))))
    (insert (concat "\n" playlist-header playlist-entries "\n"))))

(provide 'gg-org-yt)
;;; gg-org-yt.el ends here
