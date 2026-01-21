;;; preshell


(defcustom preshell-commands-alist
  '(("ping localhost" . "ping -c3 127.0.0.1"))
  "Alist of predefined shell commands for `preshell'.")


(defun preshell ()
  "Prompt for a predefined shell command in `preshell-commands-alist' and run it asynchronously."
  (interactive)
  (let* ((choice (completing-read
                  "Preshell: "
                  preshell-commands-alist
                  nil
                  t))
         (command (cdr (assoc choice preshell-commands-alist))))
    (when command
      (async-shell-command command (concat "preshell:" choice)))))


(provide 'preshell)
