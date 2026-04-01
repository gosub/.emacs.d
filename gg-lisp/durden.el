;;; durden.el --- Window tiler -*- lexical-binding: t; -*-

;;; Code:

(require 'seq)
(require 'cl-lib)


(defgroup durden nil
  "Window tiling utilities."
  :group 'windows)

(defcustom durden-excluded-buffer-names
  '("*scratch*" "*Messages*" "*Help*" "*Completions*")
  "Buffer names excluded from `durden-auto-tile'."
  :type '(repeat string)
  :group 'durden)

(defcustom durden-excluded-major-modes
  '(dired-mode)
  "Major modes excluded from `durden-auto-tile'."
  :type '(repeat symbol)
  :group 'durden)

(defcustom durden-max-windows 6
  "Maximum number of windows created by `durden-auto-tile'."
  :type 'natnum
  :group 'durden)


;;; Layout string tiler

(defun durden--parse-layout (layout)
  "Parse LAYOUT into a list of row-counts, one per column.
`|' separates columns; `-' within a column section adds a row split.
`=' expands to `--' (3 rows in one column).
`+' expands to `-|-' (2 columns each split into 2 rows)."
  (let ((s (replace-regexp-in-string
            "\\+" "-|-"
            (replace-regexp-in-string "=" "--" layout))))
    (mapcar (lambda (col)
              (1+ (seq-count (lambda (c) (= c ?-)) (string-to-list col))))
            (split-string s "|"))))

(defun durden--split-equal (window n side)
  "Split WINDOW into N equal sub-windows along SIDE (\\='right or \\='below).
Returns a list of N windows in order."
  (if (<= n 1)
      (list window)
    (let* ((total (if (eq side 'right)
                      (window-total-width window)
                    (window-total-height window)))
           (new-win (split-window window (/ total n) side)))
      (cons window (durden--split-equal new-win (1- n) side)))))

;;;###autoload
(defun durden-tile (layout)
  "Apply LAYOUT to the frame, replacing all existing windows.

The grammar is visual: `|' separates columns, `-' within a column section
separates rows in that column.

  |   column separator
  -   row separator (within a column section)
  =   shorthand for -- (three equal rows in one column)
  +   shorthand for -|- (two columns each split into two rows)

Examples:
  \"|\"     two equal columns
  \"-\"     two rows (top / bottom)
  \"-|\"    two columns; left split into two rows
  \"|-\"    two columns; right split into two rows
  \"||\"    three columns
  \"-|-\"   two columns, each split into two rows  (same as +)
  \"-||-\"  three columns; leftmost and rightmost each split into two rows
  \"--|\"   two columns; left split into three rows"
  (interactive "sLayout: ")
  (delete-other-windows)
  (let* ((row-counts (durden--parse-layout layout))
         (col-wins   (durden--split-equal (selected-window)
                                          (length row-counts)
                                          'right)))
    (cl-mapc (lambda (win nrows)
               (durden--split-equal win nrows 'below))
             col-wins row-counts)))


;;; Auto-tiler

(defun durden--main-buffer-p (buf)
  "Return non-nil if BUF should be included in auto-tiling."
  (and (buffer-live-p buf)
       (not (string-prefix-p " " (buffer-name buf)))
       (not (member (buffer-name buf) durden-excluded-buffer-names))
       (not (with-current-buffer buf
              (member major-mode durden-excluded-major-modes)))))

(defun durden--tile-recursive (window buffers)
  "Display BUFFERS across WINDOW, splitting recursively along the larger axis."
  (cond
   ((null buffers) nil)
   ((= 1 (length buffers))
    (set-window-buffer window (car buffers)))
   (t
    (let* ((w (window-total-width window))
           (h (window-total-height window))
           (half (/ (length buffers) 2))
           ;; Characters are roughly twice as tall as wide; compare w to 2h
           ;; to decide whether the window is effectively wider than it is tall
           (new-win (if (>= w (* 2 h))
                        (split-window window nil 'right)
                      (split-window window nil 'below))))
      (durden--tile-recursive window  (seq-take buffers half))
      (durden--tile-recursive new-win (seq-drop buffers half))))))

;;;###autoload
(defun durden-auto-tile ()
  "Tile all main buffers across the frame.
Buffers listed in `durden-excluded-buffer-names' or whose major mode is in
`durden-excluded-major-modes' are skipped.  At most `durden-max-windows'
windows are created.  Each split goes along the larger dimension of the
current window."
  (interactive)
  (let ((bufs (seq-take
               (seq-filter #'durden--main-buffer-p (buffer-list))
               durden-max-windows)))
    (if (null bufs)
        (message "durden: no main buffers to tile")
      (delete-other-windows)
      (durden--tile-recursive (selected-window) bufs))))


(provide 'durden)
;;; durden.el ends here
