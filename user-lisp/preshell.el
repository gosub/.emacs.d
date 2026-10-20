;;; preshell.el --- Run predefined shell commands -*- lexical-binding: t; -*-

;;; Code:

(defcustom preshell-commands-alist
  '(("ping localhost" . "ping -c3 127.0.0.1"))
  "Alist of predefined shell commands for `preshell'."
  :type '(alist :key-type string :value-type string)
  :group 'shell)

(defcustom preshell-window-height 6
  "Height in lines of the window showing the output of `preshell'."
  :type 'integer
  :group 'shell)

;;;###autoload
(defun preshell ()
  "Run a predefined shell command from `preshell-commands-alist'.
Prompt for selection and execute asynchronously, showing the output
in a small window at the bottom of the frame."
  (interactive)
  (let* ((choice (completing-read
                  "Preshell: "
                  preshell-commands-alist
                  nil
                  t))
         (command (cdr (assoc choice preshell-commands-alist))))
    (when command
      (let ((display-buffer-overriding-action
             `((display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . ,preshell-window-height))))
        (async-shell-command command (concat "preshell:" choice))))))

(provide 'preshell)
;;; preshell.el ends here
