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

(defun preshell--report-exit (proc name command)
  "Append to the buffer of PROC a line telling how it exited.
NAME is the entry of `preshell-commands-alist', COMMAND its command line."
  (let ((buffer (process-buffer proc)))
    (when (and (memq (process-status proc) '(exit signal))
               (buffer-live-p buffer))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (unless (bobp)
              (unless (bolp) (insert "\n"))
              (insert "\n"))
            (insert (format "The command \"%s\" has %s %d. Full command line: %s\n"
                            name
                            (if (eq (process-status proc) 'exit)
                                "exited with code"
                              "been terminated by signal")
                            (process-exit-status proc)
                            command))))))))

;;;###autoload
(defun preshell ()
  "Run a predefined shell command from `preshell-commands-alist'.
Prompt for selection and execute asynchronously, showing the output
in a small window at the bottom of the frame and reporting there how
the command exited."
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
        (async-shell-command command (concat "preshell:" choice)))
      (when-let* ((proc (get-buffer-process (concat "preshell:" choice))))
        (add-function :after (process-sentinel proc)
                      (lambda (proc _event)
                        (preshell--report-exit proc choice command)))))))

(provide 'preshell)
;;; preshell.el ends here
