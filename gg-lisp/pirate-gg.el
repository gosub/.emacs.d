;;; pirate-gg.el --- Media download utilities -*- lexical-binding: t; -*-

(defvar movie-pirate-destinations-gg
  '("~/dl/" "/mnt/usb/")
  "List of destination directories for `movie-pirate-gg'.")

(defun movie-pirate-gg (url name dest)
  "Asynchronously download URL to DEST/NAME.mkv using ffmpeg.
NAME is transformed to lowercase and spaces are replaced with underscores."
  (interactive
   (list
    (read-string "URL: ")
    (read-string "Name: ")
    (completing-read "Destination: " movie-pirate-destinations-gg nil t)))
  (let* ((safe-name (replace-regexp-in-string
                     " +" "_"
                     (downcase name)))
         (output (expand-file-name
                  (concat safe-name ".mkv")
                  (expand-file-name dest)))
         (process-name (format "ffmpeg-%s" safe-name)))
    (make-process
     :name            process-name
     :buffer          (format "*%s*" process-name)
     :connection-type 'pty
     :command         (list "ffmpeg" "-hide_banner"
                            "-i" url "-c" "copy" output)
     :filter          (lambda (p s)
                        (when (buffer-live-p (process-buffer p))
                          (with-current-buffer (process-buffer p)
                            (dolist (line (split-string
                                          (replace-regexp-in-string "\r" "\n" s)
                                          "\n" t))
                              (when (string-prefix-p "frame=" line)
                                (goto-char (point-max))
                                (insert line "\n")))))))))

;;;###autoload
(defalias 'pirate-movie-gg #'movie-pirate-gg)

(provide 'pirate-gg)
;;; pirate-gg.el ends here
