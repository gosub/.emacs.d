;;; cdp8-mode.el --- Text-based audio node environment -*- lexical-binding: t; -*-
;;
;; A keyboard-driven session manager for CDP8 and compatible CLI audio tools.
;; Audio nodes (generators and effects) are connected via WAV files that act as
;; patch cables. Nodes are displayed in a tabular list; wave files can be played,
;; visualised, or piped into new effect nodes.

(require 'cl-lib)
(require 'tabulated-list)
(require 'cdp8-commands)

;;; Configuration

(defgroup cdp8 nil
  "CDP8 audio node environment."
  :group 'tools)

(defcustom cdp8-binaries-dir "~/dl/audio/CDP8/NewRelease/"
  "Directory containing CDP8 binaries."
  :type 'directory
  :group 'cdp8)

(defcustom cdp8-extra-tools '()
  "Alist of (label . binary-path) for additional tools (sox, ffmpeg, ...)."
  :type '(alist :key-type string :value-type string)
  :group 'cdp8)

;;; Buffer-local session state

(defvar-local cdp8--dir nil "Absolute path of the session directory.")
(defvar-local cdp8--nodes '() "List of node plists for this session.")

;;; Session file persistence

(defun cdp8--session-file ()
  (expand-file-name "cdp8-session.el" cdp8--dir))

(defun cdp8--save ()
  (with-temp-file (cdp8--session-file)
    (let ((print-level nil) (print-length nil))
      (pp cdp8--nodes (current-buffer)))))

(defun cdp8--load ()
  (let ((f (cdp8--session-file)))
    (when (file-exists-p f)
      (with-temp-buffer
        (insert-file-contents f)
        (ignore-errors (read (current-buffer)))))))

;;; Node and binary utilities

(defun cdp8--next-id ()
  "Return the next free wave ID (a1, a2, ...)."
  (let ((ids (mapcar (lambda (n) (plist-get n :id)) cdp8--nodes))
        (n 1))
    (while (member (format "a%d" n) ids) (cl-incf n))
    (format "a%d" n)))

(defun cdp8--output-waves ()
  "List of :output values of all existing nodes."
  (mapcar (lambda (n) (plist-get n :output)) cdp8--nodes))

(defun cdp8--node-by-id (id)
  (cl-find id cdp8--nodes
           :key (lambda (n) (plist-get n :id)) :test #'string=))

(defun cdp8--binary-full-path (name)
  (let ((extra (assoc name cdp8-extra-tools)))
    (if extra (cdr extra)
      (expand-file-name name (expand-file-name cdp8-binaries-dir)))))

(defun cdp8--wav-refs (cmd)
  "Extract all *.wav filenames mentioned in CMD string."
  (let (refs)
    (with-temp-buffer
      (insert cmd)
      (goto-char (point-min))
      (while (re-search-forward "\\([[:alnum:]_./-]+\\.wav\\)" nil t)
        (push (match-string 1) refs)))
    (nreverse refs)))

(defun cdp8--resolve-cmd (cmd)
  "Expand the binary and all .wav references in CMD to absolute paths."
  (let* ((tokens (split-string cmd))
         (binary (car tokens))
         (full   (cdp8--binary-full-path binary)))
    (mapconcat
     (lambda (tok)
       (if (string-suffix-p ".wav" tok)
           (expand-file-name tok cdp8--dir)
         tok))
     (cons full (cdr tokens)) " ")))

(defun cdp8--wave-path (wave-name)
  (expand-file-name wave-name cdp8--dir))

(defun cdp8--update-status (id status)
  (setq cdp8--nodes
        (mapcar (lambda (n)
                  (if (string= (plist-get n :id) id)
                      (plist-put (copy-sequence n) :status status)
                    n))
                cdp8--nodes)))

;;; Tabulated list rendering

(defun cdp8--entries ()
  (mapcar (lambda (node)
            (list (plist-get node :id)
                  (vector
                   (plist-get node :id)
                   (symbol-name (plist-get node :type))
                   (plist-get node :cmd)
                   (mapconcat #'identity (plist-get node :inputs) " ")
                   (plist-get node :output)
                   (symbol-name (plist-get node :status)))))
          cdp8--nodes))

(defun cdp8--refresh ()
  (setq tabulated-list-entries (cdp8--entries))
  (tabulated-list-print t))

;;; Command list

(defun cdp8--all-commands ()
  "Return CDP8 commands plus user-defined extra tool labels."
  (append cdp8-commands (mapcar #'car cdp8-extra-tools)))

;;; Help side window

(defun cdp8--show-help (spec)
  "Show usage for SPEC (e.g. \"synth\" or \"synth wave\") in a side window."
  (let* ((tokens (split-string spec))
         (bin    (cdp8--binary-full-path (car tokens)))
         (args   (cdr tokens))
         (buf    (get-buffer-create "*cdp8-help*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply #'call-process bin nil t nil args)
        (special-mode)
        (goto-char (point-min))))
    (display-buffer buf '(display-buffer-in-side-window
                          (side . right) (window-width . 62)))))

;;; Node creation

(defun cdp8--register-node (type cmd)
  "Build a node plist from TYPE and CMD, append to session, save, refresh."
  (let* ((wavs   (cdp8--wav-refs cmd))
         (output (or (car (last wavs)) (concat (cdp8--next-id) ".wav")))
         (id     (file-name-sans-extension output))
         (inputs (butlast wavs))
         (node   `(:id ,id :type ,type :tool cdp8
                   :cmd ,cmd :inputs ,inputs
                   :output ,output :status pending)))
    (setq cdp8--nodes (append cdp8--nodes (list node)))
    (cdp8--save)
    (cdp8--refresh)))

(defun cdp8-new-node ()
  "Interactively create a new generator or effect node."
  (interactive)
  (let* ((type-str (completing-read "Type: " '("generator" "effect") nil t))
         (type     (intern type-str))
         (input    (when (eq type 'effect)
                     (completing-read "Input wave: " (cdp8--output-waves) nil t)))
         (spec     (completing-read "Command: " (cdp8--all-commands) nil t))
         (_        (cdp8--show-help spec))
         (next-id  (cdp8--next-id))
         (prefill  (if input
                       (format "%s %s %s.wav " spec input next-id)
                     (format "%s %s.wav " spec next-id)))
         (cmd      (read-string "Command: " prefill)))
    (cdp8--register-node type cmd)))

(defun cdp8-new-effect-from (wave-name)
  "Create a new effect node with WAVE-NAME pre-filled as input."
  (let* ((spec    (completing-read "Command: " (cdp8--all-commands) nil t))
         (_       (cdp8--show-help spec))
         (next-id (cdp8--next-id))
         (prefill (format "%s %s %s.wav " spec wave-name next-id))
         (cmd     (read-string "Command: " prefill)))
    (cdp8--register-node 'effect cmd)))

;;; Node execution

(defun cdp8--run-single (node cdp8-buf)
  "Run NODE asynchronously; update status in CDP8-BUF via sentinel."
  (let* ((id      (plist-get node :id))
         (tokens  (split-string (cdp8--resolve-cmd (plist-get node :cmd))))
         (out-buf (get-buffer-create (format "*cdp8-run: %s*" id))))
    (with-current-buffer out-buf (erase-buffer))
    (with-current-buffer cdp8-buf
      (cdp8--update-status id 'running)
      (cdp8--refresh))
    (make-process
     :name     (format "cdp8-%s" id)
     :buffer   out-buf
     :command  tokens
     :sentinel (lambda (_proc event)
                 (when (buffer-live-p cdp8-buf)
                   (with-current-buffer cdp8-buf
                     (cdp8--update-status
                      id (if (string-prefix-p "finished" event) 'done 'error))
                     (cdp8--save)
                     (cdp8--refresh)))))))

(defun cdp8-run-node ()
  "Run the node at point."
  (interactive)
  (let ((node (cdp8--node-by-id (tabulated-list-get-id))))
    (unless node (user-error "No node at point"))
    (cdp8--run-single node (current-buffer))))

;;; Run-all with topological ordering

(defun cdp8--topo-sort (nodes)
  "Sort NODES so each node appears after all nodes producing its inputs."
  (let ((sorted '()) (done '()) (remaining (copy-sequence nodes)))
    (while remaining
      (let ((ready (cl-remove-if-not
                    (lambda (n)
                      (cl-every (lambda (inp) (member inp done))
                                (plist-get n :inputs)))
                    remaining)))
        (unless ready (user-error "CDP8: circular dependency in node graph"))
        (dolist (n ready)
          (push n sorted)
          (push (plist-get n :output) done)
          (setq remaining (delq n remaining)))))
    (nreverse sorted)))

(defun cdp8--run-queue (queue cdp8-buf)
  "Run QUEUE sequentially, each node starting only after the previous succeeds."
  (when queue
    (let* ((node    (car queue))
           (rest    (cdr queue))
           (id      (plist-get node :id))
           (tokens  (split-string (cdp8--resolve-cmd (plist-get node :cmd))))
           (out-buf (get-buffer-create (format "*cdp8-run: %s*" id))))
      (with-current-buffer out-buf (erase-buffer))
      (with-current-buffer cdp8-buf
        (cdp8--update-status id 'running)
        (cdp8--refresh))
      (make-process
       :name     (format "cdp8-%s" id)
       :buffer   out-buf
       :command  tokens
       :sentinel (lambda (_proc event)
                   (when (buffer-live-p cdp8-buf)
                     (with-current-buffer cdp8-buf
                       (if (string-prefix-p "finished" event)
                           (progn
                             (cdp8--update-status id 'done)
                             (cdp8--save)
                             (cdp8--refresh)
                             (cdp8--run-queue rest cdp8-buf))
                         (progn
                           (cdp8--update-status id 'error)
                           (cdp8--save)
                           (cdp8--refresh)
                           (message "CDP8: error on %s; queue stopped" id))))))))))

(defun cdp8-run-all ()
  "Run all pending nodes in dependency order."
  (interactive)
  (let* ((pending (cl-remove-if-not
                   (lambda (n) (eq (plist-get n :status) 'pending))
                   cdp8--nodes))
         (sorted  (cdp8--topo-sort pending)))
    (if sorted
        (cdp8--run-queue sorted (current-buffer))
      (message "CDP8: no pending nodes"))))

;;; Wave actions

(defun cdp8--view-image (png title)
  "Display PNG file inline in a dedicated Emacs buffer."
  (let ((buf (get-buffer-create (format "*cdp8-view: %s*" title))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert-image (create-image png))
        (goto-char (point-min)))
      (special-mode))
    (display-buffer buf)))

(defun cdp8--play (path)
  (start-process "cdp8-play" nil "paplay" path))

(defun cdp8--waveform (path)
  (let ((out (make-temp-file "cdp8-wf" nil ".png")))
    (if (zerop (call-process "ffmpeg" nil nil nil
                             "-y" "-i" path
                             "-filter_complex"
                             "aformat=channel_layouts=mono,showwavespic=s=800x200:colors=yellow"
                             "-frames:v" "1" out))
        (cdp8--view-image out (file-name-nondirectory path))
      (message "CDP8: waveform generation failed"))))

(defun cdp8--spectrogram (path)
  (let ((out (make-temp-file "cdp8-spec" nil ".png")))
    (if (zerop (call-process "sox" nil nil nil
                             path "-n" "spectrogram"
                             "-x" "800" "-Y" "512" "-o" out))
        (cdp8--view-image out (file-name-nondirectory path))
      (message "CDP8: spectrogram generation failed"))))

(defun cdp8-wave-actions ()
  "Dispatch wave actions (play/waveform/spectrogram/new-effect) for node at point."
  (interactive)
  (let ((node (cdp8--node-by-id (tabulated-list-get-id))))
    (unless node (user-error "No node at point"))
    (let* ((wave (plist-get node :output))
           (path (cdp8--wave-path wave)))
      (unless (file-exists-p path)
        (user-error "Wave %s not found; run the node first" wave))
      (let ((key (read-key
                  (format "[p]lay [w]aveform [s]pectrogram [n]ew effect [q]uit  (%s)"
                          wave))))
        (pcase key
          (?p (cdp8--play path))
          (?w (cdp8--waveform path))
          (?s (cdp8--spectrogram path))
          (?n (cdp8-new-effect-from wave))
          (?q nil)
          (_  (message "Unknown key")))))))

;;; Node editing and deletion

(defun cdp8-edit-node ()
  "Edit the command for the node at point and mark it pending."
  (interactive)
  (let* ((id   (tabulated-list-get-id))
         (node (cdp8--node-by-id id)))
    (unless node (user-error "No node at point"))
    (let* ((new-cmd (read-string "Command: " (plist-get node :cmd)))
           (wavs    (cdp8--wav-refs new-cmd))
           (new-out (or (car (last wavs)) (plist-get node :output)))
           (new-ins (butlast wavs)))
      (setq cdp8--nodes
            (mapcar (lambda (n)
                      (if (string= (plist-get n :id) id)
                          `(:id ,id
                            :type   ,(plist-get n :type)
                            :tool   ,(plist-get n :tool)
                            :cmd    ,new-cmd
                            :inputs ,new-ins
                            :output ,new-out
                            :status pending)
                        n))
                    cdp8--nodes))
      (cdp8--save)
      (cdp8--refresh))))

(defun cdp8-delete-node ()
  "Delete the node at point, optionally deleting its wave file."
  (interactive)
  (let* ((id   (tabulated-list-get-id))
         (node (cdp8--node-by-id id)))
    (unless node (user-error "No node at point"))
    (when (yes-or-no-p (format "Delete node %s? " id))
      (let ((wave (cdp8--wave-path (plist-get node :output))))
        (when (and (file-exists-p wave)
                   (yes-or-no-p (format "Also delete %s? " (plist-get node :output))))
          (delete-file wave)))
      (setq cdp8--nodes
            (cl-remove id cdp8--nodes
                       :key (lambda (n) (plist-get n :id)) :test #'string=))
      (cdp8--save)
      (cdp8--refresh))))

;;; Mode definition

(define-derived-mode cdp8-mode tabulated-list-mode "CDP8"
  "Major mode for the CDP8 audio node environment.

Nodes are generators or effects whose outputs are WAV files (cables).
Use `cdp8-session' to open a session in a directory.

\\{cdp8-mode-map}"
  (setq tabulated-list-format
        [("ID"      6  t)
         ("Type"   10  t)
         ("Command" 36 nil)
         ("Inputs"  15 nil)
         ("Output"  12 nil)
         ("Status"   8  t)])
  (setq tabulated-list-sort-key '("ID" . nil))
  (tabulated-list-init-header)
  (define-key cdp8-mode-map (kbd "n")   #'cdp8-new-node)
  (define-key cdp8-mode-map (kbd "RET") #'cdp8-wave-actions)
  (define-key cdp8-mode-map (kbd "r")   #'cdp8-run-node)
  (define-key cdp8-mode-map (kbd "R")   #'cdp8-run-all)
  (define-key cdp8-mode-map (kbd "e")   #'cdp8-edit-node)
  (define-key cdp8-mode-map (kbd "d")   #'cdp8-delete-node)
  (define-key cdp8-mode-map (kbd "g")   #'cdp8-refresh)
  (define-key cdp8-mode-map (kbd "q")   #'quit-window))

(defun cdp8-refresh ()
  "Refresh the CDP8 session display."
  (interactive)
  (cdp8--refresh))

;;; Entry point

;;;###autoload
(defun cdp8-session (dir)
  "Open or create a CDP8 audio node session in DIR."
  (interactive (list (read-directory-name "Session directory: " default-directory)))
  (let* ((dir (expand-file-name dir))
         (_ (unless (file-directory-p dir)
              (if (yes-or-no-p (format "Directory %s does not exist. Create it? " dir))
                  (make-directory dir t)
                (user-error "Aborted"))))
         (buf (get-buffer-create
               (format "*cdp8: %s*"
                       (file-name-nondirectory (directory-file-name dir))))))
    (with-current-buffer buf
      (cdp8-mode)
      (setq cdp8--dir dir)
      (setq cdp8--nodes (or (cdp8--load) '()))
      (cdp8--refresh))
    (switch-to-buffer buf)))

(provide 'cdp8-mode)
;;; cdp8-mode.el ends here
