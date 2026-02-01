;;; gg-pirate.el --- Media download utilities -*- lexical-binding: t; -*-

(defun gg/movie-pirate (url name)
  "Asynchronously download URL to ~/dl/NAME.mkv using ffmpeg.
NAME is transformed to lowercase and spaces are replaced with underscores."
  (interactive
   (list
    (read-string "URL: ")
    (read-string "Name: ")))
  (let* ((safe-name (replace-regexp-in-string
                     " +" "_"
                     (downcase name)))
         (output (expand-file-name
                  (concat safe-name ".mkv")
                  (expand-file-name "dl" (getenv "HOME"))))
         (process-name (format "ffmpeg-%s" safe-name)))
    (start-process
     process-name
     (format "*%s*" process-name)
     "ffmpeg"
     "-i" url
     "-c" "copy"
     output)))

(provide 'gg-pirate)
;;; gg-pirate.el ends here
