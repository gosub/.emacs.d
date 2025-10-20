;;; Serendipity


(defmacro atoms-do-when (accumulator when-predicate body)
  (let ((atom (gensym "atom")))
    `(let (,accumulator)
       (mapatoms
	(lambda (,atom)
	  (when (,when-predicate ,atom)
	    (push ,atom ,accumulator))))
       ,body)))


(defun random-element (l)
  (nth (random (length l)) l))


(defun gg/describe-random-interactive-function ()
  "Display the documentation of a random interactive function."
  (interactive)
  (atoms-do-when
   all-the-funs
   (lambda (atom) (and (symbol-function atom)
		       (commandp (symbol-function atom))))
   (describe-function (random-element all-the-funs))))


(defun gg/describe-random-macro ()
  "Display the documentation of a random macro."
  (interactive)
  (atoms-do-when
   all-the-macros
   macrop
   (describe-function (random-element all-the-macros))))


(defun gg/random-emacs-info-node ()
  "Open a random emacs manual Info node in a side window."
  (interactive)
  (let* ((all-nodes (mapcar #'car (Info-toc-nodes "emacs")))
	 (nodes-count (length all-nodes))
	 (random-node (nth (random nodes-count) all-nodes)))
    (info-other-window (concat "(emacs)" random-node))))


(defun gg/describe-random-global-keybinding ()
  "Show help for a random keybinding from `global-map`."
  (interactive)
  (let* ((keys '())
         (map global-map))
    ;; Collect all keybindings from global-map
    (map-keymap
     (lambda (event binding)
       (when (and (keymapp map)
                  (commandp binding))
         (push (vector event) keys)))
     map)
    ;; Pick a random key
    (if keys
        (let* ((key (random-element keys))
               (cmd (key-binding key)))
          (describe-function cmd))
      (message "No keybindings found in `global-map`!"))))


(transient-define-prefix gg/serendip-transient ()
  "Find something about emacs, serendipitously"
  ["Find something about emacs, serendipitously" ""
   ("f" "random function" gg/describe-random-interactive-function :transient t)
   ("i" "random info node" gg/random-emacs-info-node :transient t)
   ("k" "random keybinding" gg/describe-random-global-keybinding :transient t)
   ("m" "random macro" gg/describe-random-macro :transient t)
   ("q" "quit" ignore)])


(provide 'gg-serendip)
