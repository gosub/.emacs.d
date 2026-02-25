;;; dired-prefab.el --- Run prefabricated shell commands on dired files -*- lexical-binding: t; -*-

;; Dired already lets you shell-command on marked files via `!' and `&', and
;; `dired-guess-shell-alist-user' can pre-fill the command based on file
;; extension.  That is not enough for a few reasons:
;;
;; - The pre-filled command still lands in the minibuffer for the user to
;;   confirm and edit: it's optimised for ad-hoc use, not for running a fixed
;;   workflow with zero friction.
;; - There is no support for template arguments.  Commands like
;;   `tar xf FILE -C TARGET' require a destination that varies per invocation;
;;   with dired-guess you still have to type it by hand every time.
;; - There is no filtering by file type beyond extension matching, no
;;   per-file vs. all-files-at-once distinction, and no completion over the
;;   set of applicable commands.
;;
;; dired-prefab addresses these gaps: you configure a list of named command
;; templates keyed by extension group, file/dir type, or an arbitrary
;; predicate.  Invoking `dired-prefab' on one or more marked files presents
;; only the matching commands via `completing-read', prompts for any template
;; arguments (with typed completion where appropriate), then runs the result
;; asynchronously — no editing, no confirmation.

;;; Code:

(require 'dired)

(defgroup dired-prefab nil
  "Run prefabricated shell commands on dired files."
  :group 'dired)


;;; Extension groups

(defconst dired-prefab--builtin-extension-groups
  '((images    . ("jpg" "jpeg" "png" "gif" "webp" "bmp" "tiff" "svg" "avif"))
    (video     . ("mp4" "mkv" "avi" "webm" "mov" "flv" "wmv" "m4v"))
    (audio     . ("mp3" "flac" "ogg" "wav" "aac" "m4a" "opus" "wma"))
    (archives  . ("tar" "gz" "bz2" "xz" "zst" "zip" "7z" "rar" "lz4"))
    (documents . ("pdf" "doc" "docx" "odt" "txt" "md" "rst" "tex")))
  "Built-in extension group definitions.")

(defcustom dired-prefab-extension-groups nil
  "Alist of (SYMBOL . LIST-OF-EXTENSIONS) merged with the builtin groups.
User entries take precedence over builtin entries for the same symbol."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'dired-prefab)


;;; Command lists

(defcustom dired-prefab-single-commands nil
  "Commands run once per file.
Each entry is a plist with keys:
  :name       (string, required) display name shown in completing-read
  :command    (string, required) template string; see placeholder syntax below
  :extensions list of extension strings without dot, a group symbol
              (e.g. \\='images), or nil to match any extension
  :type       \\='file (default), \\='dir, or \\='any (match both)
  :predicate  function of filename → non-nil to match, or nil (default)

Placeholder syntax in :command strings:
  %{}               the selected file(s), shell-quoted
  %{.ext}           the selected file with its extension replaced by .ext,
                    shell-quoted (e.g. %{.mp3} on song.wav → song.mp3)
  %{stem}           the selected file with its extension stripped, shell-quoted
  %{ext}            the extension of the selected file without the leading dot
  %{Label}          prompts \"Label: \" for free text
  %{file}           prompts using read-file-name
  %{existing-file}  prompts using read-file-name, must exist
  %{dir}            prompts using read-directory-name
  %{existing-dir}   prompts using read-directory-name, must exist
  %{number}         prompts using read-number
  %{Label:v1|v2}    prompts with completing-read over the listed choices"
  :type '(repeat plist)
  :group 'dired-prefab)

(defcustom dired-prefab-multi-commands nil
  "Commands run once with all marked files joined.
Same plist format as `dired-prefab-single-commands'.
%{} is substituted with all filenames space-separated and shell-quoted."
  :type '(repeat plist)
  :group 'dired-prefab)


;;; Internal helpers — extension groups

(defun dired-prefab--resolve-extensions (spec)
  "Resolve SPEC to a list of extension strings, or nil to match all.
SPEC may be nil (match all), a list of strings, or a symbol naming a group."
  (cond
   ((null spec) nil)
   ((listp spec) spec)
   ((symbolp spec)
    (or (cdr (assq spec dired-prefab-extension-groups))
        (cdr (assq spec dired-prefab--builtin-extension-groups))
        (user-error "Unknown extension group: %s" spec)))))


;;; Internal helpers — file matching

(defun dired-prefab--get-files ()
  "Return the marked files in the current dired buffer.
Signals a user-error if not in a dired buffer."
  (unless (derived-mode-p 'dired-mode)
    (user-error "Not in a dired buffer"))
  (dired-get-marked-files))

(defun dired-prefab--file-extension (filename)
  "Return the lowercased extension of FILENAME, or \"\" if none."
  (let ((ext (file-name-extension filename)))
    (if ext (downcase ext) "")))

(defun dired-prefab--matches-p (cmd filename)
  "Return non-nil if CMD plist matches FILENAME.
Checks :extensions (nil=any, list, or group symbol), :type (default \\='file),
and :predicate."
  (let ((extensions (dired-prefab--resolve-extensions (plist-get cmd :extensions)))
        (type       (or (plist-get cmd :type) 'file))
        (predicate  (plist-get cmd :predicate)))
    (and (or (null extensions)
             (member (dired-prefab--file-extension filename) extensions))
         (pcase type
           ('file (file-regular-p filename))
           ('dir  (file-directory-p filename))
           ('any  t))
         (or (null predicate)
             (funcall predicate filename)))))

(defun dired-prefab--matches-all-p (cmd files)
  "Return non-nil if CMD matches every file in FILES."
  (seq-every-p (lambda (f) (dired-prefab--matches-p cmd f)) files))


;;; Internal helpers — placeholder parsing

(defconst dired-prefab--type-keywords
  '("file" "existing-file" "dir" "existing-dir" "number")
  "Placeholder names that trigger typed input rather than free-string prompts.")

(defconst dired-prefab--auto-keywords
  '("stem" "ext")
  "Placeholder names that are auto-computed from the file, not user-prompted.")

(defun dired-prefab--placeholder-key (raw)
  "Extract the label key from RAW placeholder content.
For choice placeholders like \"Scale:50%|100%\" returns \"Scale\".
For plain labels or type keywords returns RAW as-is.
For empty string (the %{} reserved placeholder) returns \"\"."
  (let ((colon (string-search ":" raw)))
    (if colon (substring raw 0 colon) raw)))

(defun dired-prefab--parse-placeholder (raw)
  "Parse RAW placeholder content (text between %{ and }).
Returns (label . spec) where spec is one of:
  reserved          — for the empty %{} placeholder
  swap-ext          — replace file extension (auto-computed, not user-prompted)
  string            — free text input
  file              — read-file-name
  existing-file     — read-file-name, must exist
  dir               — read-directory-name
  existing-dir      — read-directory-name, must exist
  number            — read-number
  (choice v1 v2 …)  — completing-read with fixed choices"
  (cond
   ((string= raw "")
    (cons "" 'reserved))
   ((string-search ":" raw)
    (let* ((colon (string-search ":" raw))
           (label (substring raw 0 colon))
           (after (substring raw (1+ colon)))
           (options (split-string after "|")))
      (cons label (cons 'choice options))))
   ((member raw dired-prefab--type-keywords)
    (cons raw (intern raw)))
   ((string-prefix-p "." raw)
    (cons raw 'swap-ext))
   ((member raw dired-prefab--auto-keywords)
    (cons raw (intern raw)))
   (t
    (cons raw 'string))))

(defun dired-prefab--collect-placeholders (template)
  "Return (label . spec) for each unique user-prompted placeholder in TEMPLATE.
The reserved %{} is excluded. Order is by first occurrence."
  (let (result seen)
    (with-temp-buffer
      (insert template)
      (goto-char (point-min))
      (while (re-search-forward "%{\\([^}]*\\)}" nil t)
        (let* ((raw   (match-string 1))
               (entry (dired-prefab--parse-placeholder raw))
               (label (car entry))
               (spec  (cdr entry)))
          (unless (or (eq spec 'reserved) (eq spec 'swap-ext)
                      (eq spec 'stem) (eq spec 'ext)
                      (member label seen))
            (push label seen)
            (push entry result)))))
    (nreverse result)))

(defun dired-prefab--prompt-one (label spec)
  "Prompt the user for a value for LABEL using the method indicated by SPEC.
Returns a string."
  (let ((prompt (format "%s: " label)))
    (pcase spec
      ('string        (read-string prompt))
      ('file          (read-file-name prompt))
      ('existing-file (read-file-name prompt nil nil t))
      ('dir           (read-directory-name prompt))
      ('existing-dir  (read-directory-name prompt nil nil t))
      ('number        (number-to-string (read-number prompt)))
      (`(choice . ,options)
       (completing-read prompt options nil t)))))

(defun dired-prefab--prompt-for-placeholders (specs)
  "Prompt the user for each placeholder in SPECS (list of (label . spec)).
Returns an alist of (label . value)."
  (mapcar (lambda (entry)
            (cons (car entry)
                  (dired-prefab--prompt-one (car entry) (cdr entry))))
          specs))

(defun dired-prefab--expand-template (template file-or-files values
                                      &optional original-file)
  "Expand TEMPLATE, substituting %{} with FILE-OR-FILES and other placeholders.
VALUES is an alist of (label . value).
ORIGINAL-FILE, when provided, is the unquoted filename used to expand
%{.ext} swap-extension placeholders."
  (replace-regexp-in-string
   "%{\\([^}]*\\)}"
   (lambda (match)
     (let* ((raw (match-string 1 match))
            (key (dired-prefab--placeholder-key raw)))
       (cond
        ((string= key "") file-or-files)
        ((string-prefix-p "." raw)
         (if original-file
             (shell-quote-argument
              (concat (file-name-sans-extension original-file) raw))
           match))
        ((eq (cdr (dired-prefab--parse-placeholder raw)) 'stem)
         (if original-file
             (shell-quote-argument (file-name-sans-extension original-file))
           match))
        ((eq (cdr (dired-prefab--parse-placeholder raw)) 'ext)
         (if original-file
             (or (file-name-extension original-file) "")
           match))
        (t (or (cdr (assoc key values)) match)))))
   template t t))


;;; Internal helpers — command dispatch

(defun dired-prefab--run-command (name cmd-string)
  "Run CMD-STRING asynchronously in a buffer named after NAME."
  (async-shell-command cmd-string (format "*dired-prefab:%s*" name)))

(defun dired-prefab--build-candidate-list (files)
  "Build an alist of (display-name . (kind . plist)) for FILES.
For a single file, only single commands are offered.
For multiple files, multi commands come first, then per-file single commands."
  (if (= (length files) 1)
      (let ((file (car files)))
        (delq nil
              (mapcar (lambda (cmd)
                        (when (dired-prefab--matches-p cmd file)
                          (cons (plist-get cmd :name) (cons 'single cmd))))
                      dired-prefab-single-commands)))
    (let ((multi-candidates
           (delq nil
                 (mapcar (lambda (cmd)
                           (when (dired-prefab--matches-all-p cmd files)
                             (cons (plist-get cmd :name) (cons 'multi cmd))))
                         dired-prefab-multi-commands)))
          (single-candidates
           (delq nil
                 (mapcar (lambda (cmd)
                           (when (dired-prefab--matches-all-p cmd files)
                             (cons (format "per-file: %s" (plist-get cmd :name))
                                   (cons 'single cmd))))
                         dired-prefab-single-commands))))
      (append multi-candidates single-candidates))))


;;; Public command

;;;###autoload
(defun dired-prefab (&optional edit-p)
  "Run a prefabricated shell command on the marked dired file(s).
With a prefix argument, show the fully-expanded command in the minibuffer
for editing before running it."
  (interactive "P")
  (let* ((files      (dired-prefab--get-files))
         (candidates (dired-prefab--build-candidate-list files)))
    (unless candidates
      (user-error "No prefab commands match the selected file(s)"))
    (let* ((choice   (completing-read "Prefab command: " candidates nil t))
           (entry    (cdr (assoc choice candidates)))
           (kind     (car entry))
           (cmd      (cdr entry))
           (name     (plist-get cmd :name))
           (template (plist-get cmd :command))
           (specs    (dired-prefab--collect-placeholders template))
           (values   (dired-prefab--prompt-for-placeholders specs)))
      (pcase kind
        ('multi
         (let* ((files-str   (mapconcat #'shell-quote-argument files " "))
                (cmd-string  (dired-prefab--expand-template template files-str values))
                (cmd-string  (if edit-p (read-string "Command: " cmd-string) cmd-string)))
           (dired-prefab--run-command name cmd-string)))
        ('single
         (dolist (file files)
           (let* ((cmd-string (dired-prefab--expand-template template (shell-quote-argument file) values file))
                  (cmd-string (if edit-p (read-string "Command: " cmd-string) cmd-string)))
             (dired-prefab--run-command name cmd-string))))))))

(provide 'dired-prefab)
;;; dired-prefab.el ends here
