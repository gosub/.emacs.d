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
         (process-name (format "ffmpeg-%s" safe-name)))
    ;; :connection-type 'pty gives a real terminal so stderr (where ffmpeg
    ;; writes -stats) is merged with stdout and reaches the filter
    (make-process
     :name            process-name
     :buffer          (format "*%s*" process-name)
     :connection-type 'pty
     :command         (list "ffmpeg"
                            "-hide_banner" "-stats"
                            "-i" url "-c" "copy" output)
     ;; ffmpeg uses \r to overwrite the progress line in a terminal;
     ;; honour that by killing the current line before inserting the new content
     :filter  (lambda (p s)
                (when (buffer-live-p (process-buffer p))
                  (with-current-buffer (process-buffer p)
                    (let ((chunks (split-string s "\r")))
                      (goto-char (point-max))
                      (insert (car chunks))
                      (dolist (chunk (cdr chunks))
                        (goto-char (point-max))
                        (delete-region (line-beginning-position) (point))
                        (insert chunk)))))))))

(provide 'gg-pirate)
;;; gg-pirate.el ends here
