;;; gg-pirate.el --- Media download utilities -*- lexical-binding: t; -*-

(defvar gg-movie-pirate-destinations
  '("~/dl/" "/mnt/usb/")
  "List of destination directories for `gg-movie-pirate'.")

(defun gg-movie-pirate (url name dest)
  "Asynchronously download URL to DEST/NAME.mkv using ffmpeg.
NAME is transformed to lowercase and spaces are replaced with underscores."
  (interactive
   (list
    (read-string "URL: ")
    (read-string "Name: ")
    (completing-read "Destination: " gg-movie-pirate-destinations nil t)))
  (let* ((safe-name (replace-regexp-in-string
                     " +" "_"
                     (downcase name)))
         (output (expand-file-name
                  (concat safe-name ".mkv")
                  (expand-file-name dest)))
         (process-name (format "ffmpeg-%s" safe-name))
         (proc (start-process
                process-name
                (format "*%s*" process-name)
                "ffmpeg"
                "-hide_banner" "-loglevel" "error" "-stats"
                "-i" url
                "-c" "copy"
                output)))
    ;; ffmpeg uses \r to overwrite the progress line in a terminal;
    ;; convert it to \n so each update appears on its own line in the buffer
    (set-process-filter
     proc
     (lambda (p s)
       (when (buffer-live-p (process-buffer p))
         (with-current-buffer (process-buffer p)
           (goto-char (point-max))
           (insert (replace-regexp-in-string "\r" "\n" s))))))))

(provide 'gg-pirate)
;;; gg-pirate.el ends here
