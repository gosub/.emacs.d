;;; figlet.el --- Render FIGlet banners in Emacs -*- lexical-binding: t; -*-

;; Author: Giampaolo Guiducci + ChatGPT 5.2
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: fun, ascii, figlet
;; URL: https://example.com/figlet.el

;;; Commentary:
;;
;; This package parses FIGlet .flf fonts and renders text banners
;; directly inside Emacs buffers.

;;; Code:

(require 'cl-lib)

(defgroup figlet nil
  "Render FIGlet banners."
  :group 'applications)

(defcustom figlet-font-directory
  'detect
  "Directory containing FIGlet .flf font files.
Can be a directory path string, or one of the following symbols:
  `detect' - use the output of \"figlet -I2\"
  `env'    - use the FIGLET_FONTDIR environment variable"
  :type '(choice directory
                  (const :tag "Detect via figlet -I2" detect)
                  (const :tag "Use FIGLET_FONTDIR env var" env)))

(defun figlet--resolve-font-directory ()
  "Resolve `figlet-font-directory' to an actual path."
  (pcase figlet-font-directory
    ('detect
     (string-trim (shell-command-to-string "figlet -I2")))
    ('env
     (or (getenv "FIGLET_FONTDIR")
         (error "FIGLET_FONTDIR environment variable is not set")))
    ((pred stringp)
     figlet-font-directory)
    (_
     (error "Invalid `figlet-font-directory' value: %S"
            figlet-font-directory))))

(cl-defstruct figlet-font
  hardblank
  height
  smushmode
  glyphs)

(defun figlet--parse-header (line)
  "Parse FIGlet header LINE.
Return (hardblank height comment-lines smushmode)."
  (unless (string-prefix-p "flf2a" line)
    (error "Invalid FIGlet font header"))

  (let* ((parts (split-string line " +" t))
         (signature (car parts))
         (hardblank (substring signature -1))
         (height (string-to-number (nth 1 parts)))
         (old-layout (string-to-number (nth 4 parts)))
         (comment-lines (string-to-number (nth 5 parts)))
         (full-layout (and (nth 7 parts)
                           (string-to-number (nth 7 parts))))
         (smushmode (if full-layout
                        full-layout
                      (cond
                       ((= old-layout -1) 0)
                       ((= old-layout 0) 64)
                       (t (logior old-layout 128))))))
    (list hardblank height comment-lines smushmode)))

(defun figlet--strip-endmark (line endmark)
  "Strip trailing ENDMARK from LINE."
  (replace-regexp-in-string
   (concat (regexp-quote endmark) "+$") "" line))

(defun figlet-load-font (font-file)
  "Load a FIGlet font from FONT-FILE."
  (with-temp-buffer
    (insert-file-contents font-file)
    (goto-char (point-min))

    (let* ((header (buffer-substring (line-beginning-position)
                                     (line-end-position)))
           (parsed (figlet--parse-header header))
           (hardblank (nth 0 parsed))
           (height (nth 1 parsed))
           (comment-lines (nth 2 parsed))
           (smushmode (nth 3 parsed))
           (glyphs (make-hash-table :test 'eq)))

      (forward-line (1+ comment-lines))

      ;; FIGlet fonts define glyphs starting at ASCII 32
      (cl-loop for char from 32 to 126 do
               (let (lines)
                 (dotimes (_ height)
                   (let* ((raw (buffer-substring
                                (line-beginning-position)
                                (line-end-position)))
                          (endmark (substring raw -1)))
                     (push (figlet--strip-endmark raw endmark) lines)
                     (forward-line 1)))
                 (puthash char (nreverse lines) glyphs)))

      (make-figlet-font
       :hardblank hardblank
       :height height
       :smushmode smushmode
       :glyphs glyphs))))

(cl-defun figlet--smush-char (lch rch hardblank smushmode)
  "Try to smush LCH and RCH into one character.
HARDBLANK is the font's hardblank character (a one-char string).
SMUSHMODE is the font's smush mode bitfield.
Return the smushed character (a one-char string), or nil."
  ;; Space yields to anything
  (when (string= lch " ") (cl-return-from figlet--smush-char rch))
  (when (string= rch " ") (cl-return-from figlet--smush-char lch))
  ;; Smushing not enabled (bit 128)
  (when (zerop (logand smushmode 128))
    (cl-return-from figlet--smush-char nil))
  (let ((rules (logand smushmode 63)))
    ;; Universal smushing: no sub-rules active
    (when (zerop rules)
      (cl-return-from figlet--smush-char
        (cond ((string= lch hardblank) rch)
              ((string= rch hardblank) lch)
              (t rch))))
    ;; Rule 6 (bit 32): hardblank + hardblank -> hardblank
    (when (and (/= 0 (logand rules 32))
               (string= lch hardblank)
               (string= rch hardblank))
      (cl-return-from figlet--smush-char hardblank))
    ;; If either is hardblank (but not both), cannot smush
    (when (or (string= lch hardblank) (string= rch hardblank))
      (cl-return-from figlet--smush-char nil))
    ;; Rule 1 (bit 1): equal character
    (when (and (/= 0 (logand rules 1)) (string= lch rch))
      (cl-return-from figlet--smush-char lch))
    ;; Rule 2 (bit 2): underscore smushing
    (when (/= 0 (logand rules 2))
      (let ((replacers "|/\\[]{}()<>"))
        (when (and (string= lch "_") (cl-find (aref rch 0) replacers))
          (cl-return-from figlet--smush-char rch))
        (when (and (string= rch "_") (cl-find (aref lch 0) replacers))
          (cl-return-from figlet--smush-char lch))))
    ;; Rule 3 (bit 4): hierarchy
    (when (/= 0 (logand rules 4))
      (let* ((class-map '((?| . 1) (?/ . 2) (?\\ . 2)
                          (?\[ . 3) (?\] . 3) (?{ . 4) (?} . 4)
                          (?\( . 5) (?\) . 5) (?< . 6) (?> . 6)))
             (lc (alist-get (aref lch 0) class-map))
             (rc (alist-get (aref rch 0) class-map)))
        (when (and lc rc (/= lc rc))
          (cl-return-from figlet--smush-char (if (> lc rc) lch rch)))))
    ;; Rule 4 (bit 8): opposite pair -> |
    (when (/= 0 (logand rules 8))
      (let ((pairs '(("[" . "]") ("]" . "[")
                     ("{" . "}") ("}" . "{")
                     ("(" . ")") (")" . "("))))
        (when (string= (alist-get lch pairs nil nil #'string=) rch)
          (cl-return-from figlet--smush-char "|"))))
    ;; Rule 5 (bit 16): big X
    (when (/= 0 (logand rules 16))
      (cond
       ((and (string= lch "/") (string= rch "\\"))
        (cl-return-from figlet--smush-char "|"))
       ((and (string= lch "\\") (string= rch "/"))
        (cl-return-from figlet--smush-char "Y"))
       ((and (string= lch ">") (string= rch "<"))
        (cl-return-from figlet--smush-char "X"))))
    ;; No rule matched
    nil))

(defun figlet--smush-amount (output glyph hardblank smushmode)
  "Calculate how many columns GLYPH can overlap with OUTPUT.
OUTPUT and GLYPH are lists of strings (one per row).
HARDBLANK and SMUSHMODE are from the font."
  (if (zerop (logand smushmode (logior 64 128)))
      0
    (let ((max-smush (length (car glyph))))
      (cl-loop
       for out-row in output
       for glyph-row in glyph
       do
       (let* ((out-len (length out-row))
              (gl-len (length glyph-row))
              ;; Mimic C smushamt(): linebd starts at STRLEN (= out-len),
              ;; which points to the null terminator (past end in Emacs).
              ;; Loop: ch1=arr[linebd], while linebd>0 && (!ch1||ch1==' ')
              (linebd out-len)
              (ch1 0))
         (catch 'break
           (while t
             (setq ch1 (if (>= linebd out-len) 0 (aref out-row linebd)))
             (unless (and (> linebd 0) (or (zerop ch1) (= ch1 ?\s)))
               (throw 'break nil))
             (cl-decf linebd)))
         (let* ((charbd
                 (cl-loop for i from 0 below gl-len
                          unless (= (aref glyph-row i) ?\s)
                          return i
                          finally return gl-len))
                (ch2 (when (< charbd gl-len)
                       (aref glyph-row charbd)))
                (amt (+ charbd (- out-len 1 linebd))))
           (cond
            ((or (zerop ch1) (= ch1 ?\s))
             (cl-incf amt))
            (ch2
             (let ((lch (string ch1))
                   (rch (string ch2)))
               (when (figlet--smush-char lch rch hardblank smushmode)
                 (cl-incf amt)))))
           (setq max-smush (min max-smush amt)))))
      (max 0 max-smush))))

(defun figlet--render-lines (font text)
  "Render TEXT into a list of banner lines using FONT."
  (let* ((height (figlet-font-height font))
         (hardblank (figlet-font-hardblank font))
         (smushmode (figlet-font-smushmode font))
         (glyphs (figlet-font-glyphs font))
         (output (make-list height "")))

    (cl-loop
     for ch across text do
     (let* ((glyph (or (gethash ch glyphs)
                       (error "Character %c not supported by font" ch)))
            (overlap (figlet--smush-amount output glyph hardblank smushmode)))
       (dotimes (i height)
         (let* ((out-row (nth i output))
                (gl-row (nth i glyph))
                (out-len (length out-row))
                (gl-len (length gl-row))
                (keep (max 0 (- out-len overlap)))
                (result (substring out-row 0 keep)))
           ;; Apply smushing in the overlap zone.
           ;; Only process columns that fall within the output row.
           ;; Columns before the output start (negative column in C,
           ;; clamped to 0) produce '\0' via smushem and are discarded.
           (dotimes (j overlap)
             (let ((out-idx (+ keep j)))
               (when (< out-idx out-len)
                 (let* ((lch (substring out-row out-idx (1+ out-idx)))
                        (rch (if (< j gl-len)
                                 (substring gl-row j (1+ j))
                               " "))
                        (smushed (or (figlet--smush-char
                                      lch rch hardblank smushmode)
                                     lch)))
                   (setq result (concat result smushed))))))
           ;; Append the non-overlapping remainder of the glyph
           (when (< overlap gl-len)
             (setq result (concat result (substring gl-row overlap))))
           (setf (nth i output) result)))))

    ;; Replace hardblanks with spaces
    (mapcar (lambda (line)
              (replace-regexp-in-string
               (regexp-quote hardblank) " " line))
            output)))

(defun figlet--resolve-font (font)
  "Resolve FONT to an absolute .flf file path.
FONT can be a full path, or a bare name like \"standard\" which
is looked up in `figlet-font-directory'."
  (if (or (file-name-absolute-p font)
          (string-match-p "/" font)
          (string-suffix-p ".flf" font))
      font
    (expand-file-name (concat font ".flf")
                      (figlet--resolve-font-directory))))

(defun figlet-render (font text)
  "Render TEXT using FIGlet FONT and return a string.
FONT can be a font name like \"standard\" or a full path."
  (let* ((font-obj (figlet-load-font (figlet--resolve-font font)))
         (lines (figlet--render-lines font-obj text)))
    (string-join lines "\n")))

;;;###autoload
(defun figlet-insert (font text)
  "Insert a FIGlet banner using FONT for TEXT.
FONT can be a font name like \"standard\" or a full path."
  (interactive
   (list
    (read-file-name "FIGlet font: " (figlet--resolve-font-directory) nil t nil
                    (lambda (f) (string-match-p "\\.flf$" f)))
    (read-string "Text: ")))
  (insert (figlet-render font text))
  (insert "\n"))

(provide 'figlet)

;;; figlet.el ends here
