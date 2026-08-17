;;; paroliere-gg.el --- Generate made-up pronounceable words -*- lexical-binding: t; -*-

;; Builds plausible-sounding Italian-like nonsense words by assembling
;; weighted syllables: an initial syllable, one or two middle ones and a
;; final one.

(defconst paroliere-gg--vowels '("a" "e" "i" "o" "u")
  "Vowels used to build syllables.")

(defconst paroliere-gg--vowel-weights '(3 4 2 3 1)
  "Weights of `paroliere-gg--vowels'.")

(defconst paroliere-gg--bad-diphthongs
  '("ae" "ao" "au" "ea" "ei" "eo" "eu" "oa" "oe" "ou")
  "Vowel pairs that sound wrong and are never generated.")

(defconst paroliere-gg--consonants
  '("b" "c" "d" "f" "g" "l" "m" "n" "p" "r" "s" "t" "v" "z")
  "Consonants used to build syllables.")

(defconst paroliere-gg--consonant-weights '(2 4 4 4 4 4 4 4 4 4 4 4 1 1)
  "Weights of `paroliere-gg--consonants'.")

(defun paroliere-gg--weighted-choice (population weights)
  "Pick one element of POPULATION with probability proportional to WEIGHTS."
  (let ((r (random (apply #'+ weights)))
        (acc 0)
        (result nil))
    (while (and population (null result))
      (setq acc (+ acc (car weights)))
      (if (< r acc)
          (setq result (car population))
        (setq population (cdr population)
              weights (cdr weights))))
    result))

(defun paroliere-gg--choice (population)
  "Pick one element of POPULATION uniformly at random."
  (nth (random (length population)) population))

(defun paroliere-gg--simple-vowel (&optional base weights)
  "Pick a single vowel from BASE according to WEIGHTS."
  (paroliere-gg--weighted-choice (or base paroliere-gg--vowels)
                                 (or weights paroliere-gg--vowel-weights)))

(defun paroliere-gg--complex-vowel ()
  "Pick a vowel pair, avoiding `paroliere-gg--bad-diphthongs'."
  (let (pairs)
    (dolist (a paroliere-gg--vowels)
      (dolist (b paroliere-gg--vowels)
        (let ((pair (concat a b)))
          (unless (or (equal a b)
                      (member pair paroliere-gg--bad-diphthongs))
            (push pair pairs)))))
    (paroliere-gg--choice pairs)))

(defun paroliere-gg--vowel ()
  "Pick a vowel, mostly simple and occasionally a pair."
  (paroliere-gg--weighted-choice (list (paroliere-gg--simple-vowel)
                                       (paroliere-gg--complex-vowel))
                                 '(5 1)))

(defun paroliere-gg--simple-consonant (&optional base weights)
  "Pick a single consonant from BASE according to WEIGHTS."
  (paroliere-gg--weighted-choice (or base paroliere-gg--consonants)
                                 (or weights paroliere-gg--consonant-weights)))

(defun paroliere-gg--complex-consonant ()
  "Pick a consonant cluster."
  (let ((c1 (paroliere-gg--simple-consonant))
        (c2 (paroliere-gg--simple-consonant
             '("b" "c" "d" "f" "g" "l" "m" "n" "p" "t") (make-list 10 1)))
        (c3 (paroliere-gg--simple-consonant
             '("b" "c" "d" "f" "g" "p" "t") (make-list 7 1))))
    (paroliere-gg--choice (list (concat c1 c1)
                                (concat "r" c2)
                                (concat "s" c3)
                                (concat c3 "r")))))

(defun paroliere-gg--consonant (&optional initial)
  "Pick a consonant.  When INITIAL is non-nil never pick a cluster."
  (if initial
      (paroliere-gg--simple-consonant)
    (paroliere-gg--weighted-choice (list (paroliere-gg--simple-consonant)
                                         (paroliere-gg--complex-consonant))
                                   '(5 1))))

(defun paroliere-gg--syllable ()
  "Build a middle syllable."
  (concat (paroliere-gg--consonant) (paroliere-gg--vowel)))

(defun paroliere-gg--initial-syllable ()
  "Build an initial syllable, sometimes preceded by a vowel."
  (concat (paroliere-gg--weighted-choice
           (list "" (paroliere-gg--simple-vowel)) '(5 1))
          (paroliere-gg--consonant t)
          (paroliere-gg--vowel)))

(defun paroliere-gg--final-syllable ()
  "Build a final syllable, sometimes ending on a consonant."
  (let ((v (paroliere-gg--simple-vowel '("a" "e" "o") '(1 1 1))))
    (concat (paroliere-gg--consonant)
            (paroliere-gg--weighted-choice (list "" v) '(1 2)))))

(defun paroliere-gg-word ()
  "Return one made-up word."
  (let ((syllables (+ 1 (random 2)))
        (word (paroliere-gg--initial-syllable)))
    (dotimes (_ syllables)
      (setq word (concat word (paroliere-gg--syllable))))
    (concat word (paroliere-gg--final-syllable))))

(defun paroliere-gg (&optional count)
  "Generate COUNT made-up words, 7 by default.
Interactively, a numeric prefix argument sets COUNT.  The words are
shown in the echo area and returned as an upcased, space separated
string."
  (interactive "P")
  (let ((n (if count (prefix-numeric-value count) 7))
        (words nil))
    (dotimes (_ n)
      (push (paroliere-gg-word) words))
    (let ((line (upcase (mapconcat #'identity words " "))))
      (when (called-interactively-p 'interactive)
        (message "%s" line))
      line)))

(provide 'paroliere-gg)
;;; paroliere-gg.el ends here
