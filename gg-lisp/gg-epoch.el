;;; Epoch conversion


(defun gg/epoch-to-timestamp (epoch)
  "Convert an integer UNIX EPOCH time to a human-readable timestamp.

When called interactively, use the active region or the symbol at
point as the default epoch value if it consists only of digits.
Display the corresponding local time in the echo area, and return
the formatted string."
  (interactive
   (let* ((text
           (if (use-region-p)
               (buffer-substring-no-properties
                (region-beginning) (region-end))
             (thing-at-point 'symbol t)))
          (default
           (and text (string-match-p "\\`[0-9]+\\'" text)
                text)))
     (list (read-number "epoch: " (and default (string-to-number default))))))
  (message
   (format-time-string "%Y-%m-%d %H:%M:%S"
                       (seconds-to-time epoch))))


(provide 'gg-epoch)
