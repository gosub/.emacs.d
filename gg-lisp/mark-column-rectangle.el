;;; mark-column-rectangle.el --- Mark rectangular region around text column under cursor

;;; Commentary:
;; This package provides a function to mark a rectangular region around
;; the column of text under the cursor. The column is delimited:
;; - Horizontally: by whitespace, BOL, or EOL
;; - Vertically: by whitespace, empty lines, or buffer boundaries

;;; Code:

(defun mcr--current-word-bounds ()
  "Return (START . END) column positions of word at point, or nil if on whitespace."
  (save-excursion
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position))
          (orig-point (point)))
      ;; Check if we're on whitespace or empty line
      (when (and (not (eolp))
                 (not (looking-at-p "[ \t]")))
        (let (start-col end-col)
          ;; Find start of word
          (skip-chars-backward "^ \t" line-start)
          (setq start-col (current-column))
          ;; Find end of word
          (goto-char orig-point)
          (skip-chars-forward "^ \t" line-end)
          (setq end-col (current-column))
          (cons start-col end-col))))))

(defun mcr--get-words-on-line ()
  "Return list of (START-COL . END-COL) for all words on current line."
  (save-excursion
    (let ((line-end (line-end-position))
          words)
      (beginning-of-line)
      (while (< (point) line-end)
        (skip-chars-forward " \t" line-end)
        (when (< (point) line-end)
          (let ((word-start-col (current-column)))
            (skip-chars-forward "^ \t" line-end)
            (push (cons word-start-col (current-column)) words))))
      (nreverse words))))

(defun mcr--ranges-overlap-p (start1 end1 start2 end2)
  "Return t if range [START1, END1) overlaps with [START2, END2)."
  (and (< start1 end2) (< start2 end1)))

(defun mcr--find-overlapping-word (words start-col end-col)
  "Find a word in WORDS list that overlaps with column range [START-COL, END-COL).
Returns (WORD-START . WORD-END) or nil."
  (catch 'found
    (dolist (word words)
      (when (mcr--ranges-overlap-p (car word) (cdr word) start-col end-col)
        (throw 'found word)))
    nil))

(defun mcr--line-has-overlapping-word (start-col end-col)
  "Return t if current line has a word overlapping column range [START-COL, END-COL)."
  (let ((words (mcr--get-words-on-line)))
    (mcr--find-overlapping-word words start-col end-col)))

(defun mcr--find-column-boundaries (start-col end-col)
  "Find vertical boundaries of text column overlapping [START-COL, END-COL).
Returns (TOP-LINE . BOTTOM-LINE) as line numbers."
  (let ((current-line (line-number-at-pos))
        top-line bottom-line)
    ;; Search upward
    (save-excursion
      (setq top-line current-line)
      (while (and (= (forward-line -1) 0)
                  (mcr--line-has-overlapping-word start-col end-col))
        (setq top-line (line-number-at-pos))))
    ;; Search downward
    (save-excursion
      (setq bottom-line current-line)
      (while (and (= (forward-line 1) 0)
                  (not (eobp))
                  (mcr--line-has-overlapping-word start-col end-col))
        (setq bottom-line (line-number-at-pos)))
      ;; Check if we're at eobp but the line still has content
      (when (and (eobp) 
                 (mcr--line-has-overlapping-word start-col end-col)
                 (> (line-number-at-pos) bottom-line))
        (setq bottom-line (line-number-at-pos))))
    (cons top-line bottom-line)))

(defun mcr--find-rectangle-columns (top-line bottom-line start-col end-col)
  "Find the leftmost and rightmost columns of words overlapping [START-COL, END-COL).
Searches from TOP-LINE to BOTTOM-LINE.
Returns (LEFT-COL . RIGHT-COL)."
  (let ((left-col most-positive-fixnum)
        (right-col 0))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- top-line))
      (dotimes (_ (1+ (- bottom-line top-line)))
        (let* ((words (mcr--get-words-on-line))
               (overlapping (mcr--find-overlapping-word words start-col end-col)))
          (when overlapping
            (setq left-col (min left-col (car overlapping)))
            (setq right-col (max right-col (cdr overlapping)))))
        (forward-line 1)))
    (cons left-col right-col)))

(defun mcr--expand-boundaries (top-line bottom-line left-col right-col)
  "Iteratively expand boundaries until stable.
Some words found by expanding horizontally might allow further vertical expansion.
Returns (TOP-LINE BOTTOM-LINE LEFT-COL RIGHT-COL)."
  (let ((changed t)
        (iterations 0)
        (max-iterations 100))  ; Safety limit
    (while (and changed (< iterations max-iterations))
      (setq changed nil)
      (setq iterations (1+ iterations))
      ;; Try to expand vertically with current column range
      (let ((new-vert (mcr--find-column-boundaries left-col right-col)))
        (save-excursion
          ;; Need to position at a line within current range for the search
          (goto-char (point-min))
          (forward-line (1- top-line))
          (let ((vert-bounds (mcr--find-column-boundaries left-col right-col)))
            (when (or (< (car vert-bounds) top-line)
                      (> (cdr vert-bounds) bottom-line))
              (setq top-line (min top-line (car vert-bounds)))
              (setq bottom-line (max bottom-line (cdr vert-bounds)))
              (setq changed t)))))
      ;; Try to expand horizontally with current line range
      (let ((new-horiz (mcr--find-rectangle-columns top-line bottom-line left-col right-col)))
        (when (or (< (car new-horiz) left-col)
                  (> (cdr new-horiz) right-col))
          (setq left-col (min left-col (car new-horiz)))
          (setq right-col (max right-col (cdr new-horiz)))
          (setq changed t))))
    (list top-line bottom-line left-col right-col)))

;;;###autoload
(defun mark-column-rectangle ()
  "Mark a rectangular region around the text column under cursor.
The column is delimited:
- Horizontally: by whitespace, beginning of line, or end of line
- Vertically: by lines without text at this column, empty lines,
  or buffer boundaries.

Uses `rectangle-mark-mode' to mark the region.
The rectangle width accommodates the longest word in the column."
  (interactive)
  (let ((word-bounds (mcr--current-word-bounds)))
    (unless word-bounds
      (user-error "Cursor is not on a word"))
    (let* ((start-col (car word-bounds))
           (end-col (cdr word-bounds))
           ;; Find initial vertical boundaries
           (line-boundaries (mcr--find-column-boundaries start-col end-col))
           (top-line (car line-boundaries))
           (bottom-line (cdr line-boundaries))
           ;; Find initial horizontal boundaries
           (col-boundaries (mcr--find-rectangle-columns top-line bottom-line start-col end-col))
           (left-col (car col-boundaries))
           (right-col (cdr col-boundaries))
           ;; Expand iteratively until stable
           (final-bounds (mcr--expand-boundaries top-line bottom-line left-col right-col)))
      (setq top-line (nth 0 final-bounds))
      (setq bottom-line (nth 1 final-bounds))
      (setq left-col (nth 2 final-bounds))
      (setq right-col (nth 3 final-bounds))
      ;; Move to top-left corner
      (goto-char (point-min))
      (forward-line (1- top-line))
      (move-to-column left-col)
      ;; Set mark
      (push-mark (point) t t)
      ;; Move to bottom-right corner
      (goto-char (point-min))
      (forward-line (1- bottom-line))
      (move-to-column right-col)
      ;; Activate rectangle mark mode
      (rectangle-mark-mode 1)
      (message "Marked column rectangle: lines %d-%d, columns %d-%d"
               top-line bottom-line left-col right-col))))

(provide 'mark-column-rectangle)
;;; mark-column-rectangle.el ends here
