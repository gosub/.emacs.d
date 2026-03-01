;;; drench.el --- Play a clone of the Drench game in Emacs  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)


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


(defmacro drench-define-face+var (number bg-color)
  (let ((sym (intern (format "drench-face-%d" number))))
    `(progn
       (defface ,sym
       '((t (:background ,bg-color :foreground "white")))
       ,(format "Face for number %d in drench game." number)
       :group 'drench-faces)
     )))


(drench-define-face+var 1 "red")
(drench-define-face+var 2 "dark blue")
(drench-define-face+var 3 "dark green")
(drench-define-face+var 4 "dark orange")
(drench-define-face+var 5 "dark magenta")
(drench-define-face+var 6 "dark grey")


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
  (font-lock-add-keywords
   nil
   '(("1" (0 'drench-face-1))
     ("2" (0 'drench-face-2))
     ("3" (0 'drench-face-3))
     ("4" (0 'drench-face-4))
     ("5" (0 'drench-face-5))
     ("6" (0 'drench-face-6)))))


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
	(insert (number-to-string (drench-get-square row col))))
      (insert "\n"))
    (insert "\n\nmoves left: "
	    (number-to-string
	     (drench-remaining-moves))
	    "\n")))


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
