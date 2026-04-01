;;; durden.el --- Window tiler -*- lexical-binding: t; -*-

;;; Code:

(require 'seq)


(defgroup durden nil
  "Window tiling utilities."
  :group 'windows)

(defcustom durden-excluded-buffer-names
  '("*scratch*" "*Messages*")
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

(defun durden--apply-layout (window chars)
  "Split WINDOW according to CHARS (a list of characters).
Characters are consumed in depth-first order: the first char splits the
root window, then each resulting sub-window recursively consumes the next
character.  Returns the list of unconsumed characters."
  (if (null chars)
      nil
    (let ((c (car chars))
          (rest (cdr chars)))
      (pcase c
        (?-
         ;; Split top / bottom
         (let ((below (split-window window nil 'below)))
           (setq rest (durden--apply-layout window rest))
           (setq rest (durden--apply-layout below rest))
           rest))
        (?|
         ;; Split left / right
         (let ((right (split-window window nil 'right)))
           (setq rest (durden--apply-layout window rest))
           (setq rest (durden--apply-layout right rest))
           rest))
        (?=
         ;; Split into three horizontal strips of equal height
         (let* ((h (window-total-height window))
                (third (max 3 (/ h 3)))
                (mid (split-window window third 'below))
                (bot (split-window mid   third 'below)))
           (setq rest (durden--apply-layout window rest))
           (setq rest (durden--apply-layout mid   rest))
           (setq rest (durden--apply-layout bot   rest))
           rest))
        (?+
         ;; Split into a 2×2 grid
         (let* ((right    (split-window window nil 'right))
                (bot-left (split-window window nil 'below))
                (bot-right (split-window right  nil 'below)))
           (setq rest (durden--apply-layout window    rest))
           (setq rest (durden--apply-layout right     rest))
           (setq rest (durden--apply-layout bot-left  rest))
           (setq rest (durden--apply-layout bot-right rest))
           rest))
        (_
         ;; Unknown character: treat this window as a leaf
         rest)))))

;;;###autoload
(defun durden-tile (layout)
  "Apply LAYOUT to the current frame, replacing all existing windows.

LAYOUT is a string; each character describes how to split one window:

  -  split top / bottom
  |  split left / right
  =  split into three horizontal strips
  +  split into a 2×2 grid

Characters are applied in depth-first order.  The first character splits
the full frame; each resulting sub-window then consumes the next character
in turn.  Windows that receive no character are left as leaves.

Examples:
  \"-\"   two rows
  \"|\"   two columns
  \"|-\"  two columns; left column split into two rows
  \"-|\"  two rows; top row split into two columns
  \"||\"  three columns (left column split again)
  \"+\"   2×2 grid"
  (interactive "sLayout: ")
  (delete-other-windows)
  (durden--apply-layout (selected-window) (string-to-list layout)))


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
      (durden--tile-recursive window   (seq-take buffers half))
      (durden--tile-recursive new-win  (seq-drop buffers half))))))

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
