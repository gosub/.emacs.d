;;; drench.el --- Play a clone of the Drench game in Emacs  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)


(defgroup drench nil
  "Drench game."
  :group 'games)

(defcustom drench-cell-size 20
  "Pixel size of each board cell in graphical display mode."
  :type 'natnum
  :group 'drench)


(defvar *drench-buffer-name* "*drench*"
  "Name of the drench game buffer.")

(defvar *drench-board* nil
  "The game board.")

(defvar *drench-board-size* 14
  "The size of the game board.")

(defvar *drench-level* nil
  "The current game level.")

(defvar *drench-max-moves* 30
  "How many moves the player as on the first level.")

(defvar *drench-moves-done* nil
  "How many moves we have done during the current level.")


;;;###autoload
(defun drench ()
  "Start playing drench."
  (interactive)
  (switch-to-buffer *drench-buffer-name*)
  (drench-mode)
  (drench-init-level 1))


(defmacro drench-define-face (number dark-color light-color)
  `(defface ,(intern (format "drench-face-%d" number))
     '((((background dark)) :background ,dark-color :foreground "white")
       (t                   :background ,light-color :foreground "white"))
     ,(format "Face for color %d in drench." number)
     :group 'drench-faces))

(drench-define-face 1 "#cc4444" "#bb2222")
(drench-define-face 2 "#4466cc" "#2244bb")
(drench-define-face 3 "#44aa55" "#227733")
(drench-define-face 4 "#cc7722" "#aa5500")
(drench-define-face 5 "#9944cc" "#7722aa")
(drench-define-face 6 "#229999" "#117777")

(defconst drench-digit-chars
  ["₁" "₂" "₃" "₄" "₅" "₆"]
  "Subscript digit strings for board values 1–6.")

(defconst drench-face-syms
  [nil drench-face-1 drench-face-2 drench-face-3
       drench-face-4 drench-face-5 drench-face-6]
  "Face symbols indexed by board value (index 0 unused).")



(define-derived-mode drench-mode special-mode "drench"
  (define-key drench-mode-map (kbd "1")
    (lambda () (interactive) (drench-fill 1)))
  (define-key drench-mode-map (kbd "2")
    (lambda () (interactive) (drench-fill 2)))
  (define-key drench-mode-map (kbd "3")
    (lambda () (interactive) (drench-fill 3)))
  (define-key drench-mode-map (kbd "4")
    (lambda () (interactive) (drench-fill 4)))
  (define-key drench-mode-map (kbd "5")
    (lambda () (interactive) (drench-fill 5)))
  (define-key drench-mode-map (kbd "6")
    (lambda () (interactive) (drench-fill 6)))
  (define-key drench-mode-map (kbd "q")
    'drench-quit-game)
  (define-key drench-mode-map [mouse-1] #'drench-mouse-select))


(defun drench-mouse-select (event)
  "Select color by clicking a board cell."
  (interactive "e")
  (let* ((pos  (event-start event))
         (pt   (posn-point pos))
         (cell (and pt (get-text-property pt 'drench-cell))))
    (when cell
      (drench-fill (drench-get-square (car cell) (cdr cell))))))


(defun drench-random-board ()
  (let* ((size (* *drench-board-size*
		 *drench-board-size*))
	(board (make-vector size 0)))
    (dotimes (index size)
      (aset board index (+ 1 (random 6))))
    board))


(defun drench-max-moves-current-level ()
 (+ 1 (- *drench-max-moves* *drench-level*)))


(defun drench-init-level (lvl)
  "Start a new level of drench."
  (setq *drench-board* (drench-random-board))
  (setq *drench-level* lvl)
  (setq *drench-moves-done* 0)
  (drench-print-board))


(defun drench-print-board ()
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dotimes (row *drench-board-size*)
      (dotimes (col *drench-board-size*)
        (let* ((val  (drench-get-square row col))
               (face (aref drench-face-syms val)))
          (if (display-images-p)
              (insert (propertize " "
                                  'face face
                                  'display `(space :width (,drench-cell-size)
                                                   :height (,drench-cell-size))
                                  'drench-cell (cons row col)))
            (let ((dig (aref drench-digit-chars (1- val))))
              (insert (propertize dig 'face face 'drench-cell (cons row col)))
              (insert (propertize " " 'face face 'drench-cell (cons row col)))))))
      (insert "\n"))
    (insert "\n")
    (dotimes (i 6)
      (let* ((val  (1+ i))
             (face (aref drench-face-syms val)))
        (insert (propertize (format " %d " val) 'face face))))
    (insert (format "\nmoves left: %d\n" (drench-remaining-moves)))))


(defun drench-get-square (row column)
  (elt *drench-board*
       (+ column (* row *drench-board-size*))))


(defun drench-set-square (row column value)
  (aset *drench-board*
	(+ column (* row *drench-board-size*))
	value))


(defun drench-remaining-moves ()
  (- (drench-max-moves-current-level)
     *drench-moves-done*))


(defun drench-board-filled? ()
  (cl-loop with f = (elt *drench-board* 1)
	   for e across *drench-board*
	   if (/= e f) return nil
	   finally return 1))


(defun drench-flood-fill (value)
  (let ((tovisit (list (list 0 0)))
        (visited (make-hash-table :test 'equal))
        (similar (drench-get-square 0 0)))
    (while tovisit
      (let* ((pos (pop tovisit))
             (x (car pos))
             (y (cadr pos)))
        (drench-set-square x y value)
        (puthash pos t visited)
        (dolist (neighbor (list (list (1+ x) y) (list (1- x) y)
                                (list x (1+ y)) (list x (1- y))))
          (let ((nx (car neighbor)) (ny (cadr neighbor)))
            (when (and (not (gethash neighbor visited))
                       (<= 0 nx) (< nx *drench-board-size*)
                       (<= 0 ny) (< ny *drench-board-size*)
                       (= similar (drench-get-square nx ny)))
              (puthash neighbor t visited)
              (push neighbor tovisit))))))))


(defun drench-fill (value)
  (drench-flood-fill value)
  (setq *drench-moves-done*
	(+ *drench-moves-done* 1))
  (drench-print-board)
  (drench-evaluate-endgame))


(defun drench-evaluate-endgame ()
  (cond
    ((drench-board-filled?)
     (beep)
     (if (y-or-n-p (format "You completed level %d, continue?" *drench-level*))
         (drench-init-level (+ 1 *drench-level*))
       (drench-quit-game)))
    ((= (drench-remaining-moves) 0)
     (if (y-or-n-p (format "You lost at level %d, new-game?" *drench-level*))
         (drench-init-level 1)
       (drench-quit-game)))))


(defun drench-quit-game ()
  (interactive)
  (kill-buffer *drench-buffer-name*))

(provide 'drench)

;;; drench.el ends here
