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
  "/usr/share/figlet"
  "Directory containing FIGlet .flf font files."
  :type 'directory)

(cl-defstruct figlet-font
  hardblank
  height
  glyphs)

(defun figlet--parse-header (line)
  "Parse FIGlet header LINE."
  (unless (string-prefix-p "flf2a" line)
    (error "Invalid FIGlet font header"))

  (let* ((parts (split-string line " +" t))
         (signature (car parts))
         (hardblank (substring signature -1))
         (height (string-to-number (nth 1 parts)))
         (comment-lines (string-to-number (nth 5 parts))))
    (list hardblank height comment-lines)))

(defun figlet--strip-endmark (line endmark)
  "Strip ENDMARK from LINE."
  (replace-regexp-in-string
   (regexp-quote endmark) "" line t t))

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
       :glyphs glyphs))))

(defun figlet--render-lines (font text)
  "Render TEXT into a list of banner lines using FONT."
  (let* ((height (figlet-font-height font))
         (hardblank (figlet-font-hardblank font))
         (glyphs (figlet-font-glyphs font))
         (output (make-list height "")))

    (cl-loop for ch across text do
             (let ((glyph (gethash ch glyphs)))
               (unless glyph
                 (error "Character %c not supported by font" ch))
               (dotimes (i height)
                 (setf (nth i output)
                       (concat (nth i output)
                               (nth i glyph))))))

    ;; Replace hardblanks with spaces
    (mapcar (lambda (line)
              (replace-regexp-in-string
               (regexp-quote hardblank) " " line))
            output)))

(defun figlet-render (font-file text)
  "Render TEXT using FIGlet FONT-FILE and return a string."
  (let* ((font (figlet-load-font font-file))
         (lines (figlet--render-lines font text)))
    (string-join lines "\n")))

;;;###autoload
(defun figlet-insert (font-name text)
  "Insert a FIGlet banner using FONT-NAME for TEXT."
  (interactive
   (list
    (read-file-name "FIGlet font: " figlet-font-directory nil t nil
                    (lambda (f) (string-match-p "\\.flf$" f)))
    (read-string "Text: ")))
  (insert (figlet-render font-name text))
  (insert "\n"))

(provide 'figlet)

;;; figlet.el ends here
