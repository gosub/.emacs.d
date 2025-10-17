(defun gg/random-emacs-info-node ()
  "Open a random emacs manual Info node in a side window."
  (interactive)
  (let* ((all-nodes (mapcar #'car (Info-toc-nodes "emacs")))
	 (nodes-count (length all-nodes))
	 (random-node (nth (random nodes-count) all-nodes)))
    (info-other-window (concat "(emacs)" random-node))))


(defun gg/describe-random-interactive-function ()
  "Display the documentation of a random interactive function."
  (interactive)
  (let (all-the-funs)
    (mapatoms (lambda (sym)
		(when (and (symbol-function sym)
			   (commandp (symbol-function sym)))
		  (push sym all-the-funs))))
    (describe-function
     (nth (random (length all-the-funs))
	  all-the-funs))))


(provide 'gg-serendip)
