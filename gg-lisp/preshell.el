;;; preshell.el --- Run predefined shell commands -*- lexical-binding: t; -*-

;;; Code:

(defcustom preshell-commands-alist
  '(("ping localhost" . "ping -c3 127.0.0.1"))
  "Alist of predefined shell commands for `preshell'."
  :type '(alist :key-type string :value-type string)
  :group 'shell)

(defun preshell ()
  "Run a predefined shell command from `preshell-commands-alist'.
Prompt for selection and execute asynchronously."
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
;;; preshell.el ends here
