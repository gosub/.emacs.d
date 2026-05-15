(defun gg/sclang-eval-dwim ()
  "If a region is active, eval it.
Else if point is inside a `(...)' SC code block whose `(' is at the
beginning of a line, mark the block and eval it.
Otherwise eval the current line."
  (interactive)
  (if (use-region-p)
      (gg--sclang-eval-region)
    (let ((reg (gg--enclosing-sc-block)))
      (if reg
          (save-excursion
            (goto-char (car reg))
            (set-mark (point))
            (goto-char (cadr reg))
            (gg--sclang-eval-region))
        (gg--sclang-eval-line)))))


(defun gg--sclang-eval-region ()
  (pulse-momentary-highlight-region
   (region-beginning) (region-end))
  (sclang-eval-region)
  (deactivate-mark))


(defun gg--sclang-eval-line ()
  (pulse-momentary-highlight-one-line (point))
  (sclang-eval-line))


(defun gg--enclosing-sc-block ()
  "Return (START END) of the SC code block enclosing point, or nil.
A block is a `(' at the beginning of a line and its matching `)',
located via `forward-sexp' so nesting, strings, and comments balance
correctly."
  (save-excursion
    (let ((origin (point))
          start end)
      (when (or (and (bolp) (looking-at "($"))
                (re-search-backward "^($" nil t))
        (setq start (point))
        (condition-case nil
            (progn (forward-sexp 1)
                   (setq end (point)))
          (scan-error nil)))
      (when (and start end (<= start origin) (<= origin end))
        (list start end)))))


(defun gg/sclang-dired-recordings-dir ()
  "Open a dired buffer on the Platform Recordings directory."
  (interactive)
  (dired (sclang-eval-sync "Platform.recordingsDir")))

(require 'transient)

(transient-define-prefix gg/sclang-transient ()
  "Transient for sclang-mode common options."
  [["Control"
    ("s" "Start SClang" sclang-start :transient t)
    ("q" "Stop SCLang" sclang-stop)
    ("k" "Kill SCLang" sclang-kill)
    ("l" "Recompile classes"  sclang-recompile)]
   ["Server"
    ("b" "Boot server" sclang-server-boot)
    ("t" "Quit server" sclang-server-quit)]
   ["Navigation"
    ("w" "Switch to Workspace" sclang-switch-to-workspace)
    ("p" "Show Post" sclang-show-post-buffer)
    ("c" "Clear Post" sclang-clear-post-buffer)]
   ["Help"
    ("h" "Help GUI" sclang-open-help-gui)
    ("f" "Find help in GUI" sclang-find-help-in-gui)]
   ["Record"
    ("rn" "Prepare for record" sclang-server-prepare-for-record)
    ("rr" "Start recording" sclang-server-record)
    ("rs" "Stop recording" sclang-server-stop-recording)
    ("rp" "Pause recording" sclang-server-pause-recording)
    ("rd" "Recordings dir" gg/sclang-dired-recordings-dir)]])


(provide 'gg-sclang-aux)
