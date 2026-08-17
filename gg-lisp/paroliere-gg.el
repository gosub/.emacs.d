;;; paroliere-gg.el --- Generate made-up pronounceable words -*- lexical-binding: t; -*-

;; Builds plausible-sounding Romance nonsense words (Italian, Latin and
;; Spanish flavoured) by assembling syllables that respect Romance
;; phonotactics: simple or muta-cum-liquida onsets, a vowel or a real
;; diphthong as nucleus, only the clusters those languages actually allow
;; across a syllable boundary, and a word ending on a vowel or on one of
;; the few consonants Romance words may close with.

(defconst paroliere-gg--vowels '("a" "e" "i" "o" "u")
  "Vowels used as syllable nuclei.")

(defconst paroliere-gg--vowel-weights '(5 4 2 4 1)
  "Weights of `paroliere-gg--vowels'.")

(defconst paroliere-gg--diphthongs
  '("ia" "ie" "io" "iu" "ua" "ue" "ui" "uo" "ai" "au" "ei" "oi")
  "Diphthongs that actually occur in Romance languages.")

(defconst paroliere-gg--final-vowels '("a" "e" "i" "o")
  "Vowels a word may end on.")

(defconst paroliere-gg--final-vowel-weights '(6 4 1 6)
  "Weights of `paroliere-gg--final-vowels'.")

(defconst paroliere-gg--final-consonants '("n" "s" "r" "l")
  "Consonants a word may end on, as in Latin and Spanish.")

(defconst paroliere-gg--consonants
  '("b" "c" "d" "f" "g" "l" "m" "n" "p" "r" "s" "t" "v" "z")
  "Consonants usable as a simple syllable onset.")

(defconst paroliere-gg--consonant-weights '(2 4 4 2 2 4 4 4 3 4 3 4 2 1)
  "Weights of `paroliere-gg--consonants'.")

(defconst paroliere-gg--clusters
  '("br" "cr" "dr" "fr" "gr" "pr" "tr" "bl" "cl" "fl" "gl" "pl")
  "Muta cum liquida onsets.")

(defconst paroliere-gg--geminable
  '("b" "c" "d" "f" "g" "l" "m" "n" "p" "r" "s" "t" "z")
  "Consonants that may be doubled between vowels, as in Italian.")

(defconst paroliere-gg--coda-onsets
  '(("n" "c" "d" "f" "g" "s" "t" "v" "z")
    ("m" "b" "p")
    ("r" "b" "c" "d" "f" "g" "l" "m" "n" "p" "s" "t" "v" "z")
    ("s" "c" "p" "t")
    ("l" "b" "c" "d" "g" "m" "p" "t" "v" "z"))
  "Alist of coda consonant to the onsets it may precede.
Each entry is (CODA . ONSETS), spelling out the consonant pairs Romance
languages allow across a syllable boundary.  Note the split between N
and M: Romance spelling writes M, not N, before B and P.")

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

(defun paroliere-gg--vowel ()
  "Pick a single vowel."
  (paroliere-gg--weighted-choice paroliere-gg--vowels
                                 paroliere-gg--vowel-weights))

(defun paroliere-gg--simple-onset ()
  "Pick a single consonant."
  (paroliere-gg--weighted-choice paroliere-gg--consonants
                                 paroliere-gg--consonant-weights))

(defun paroliere-gg--linked-onset ()
  "Pick an onset preceded by a coda, either a geminate or a legal pair."
  (let ((geminate (paroliere-gg--choice paroliere-gg--geminable))
        (pair (paroliere-gg--choice paroliere-gg--coda-onsets)))
    (paroliere-gg--weighted-choice
     (list (concat geminate geminate)
           (concat (car pair) (paroliere-gg--choice (cdr pair))))
     '(1 1))))

(defun paroliere-gg--onset (&optional no-coda)
  "Pick a syllable onset, mostly a single consonant.
With NO-CODA never pick an onset carrying a coda on its left, as is
required word initially and after a diphthong."
  (if no-coda
      (paroliere-gg--weighted-choice
       (list (paroliere-gg--simple-onset)
             (paroliere-gg--choice paroliere-gg--clusters))
       '(7 1))
    (paroliere-gg--weighted-choice
     (list (paroliere-gg--simple-onset)
           (paroliere-gg--choice paroliere-gg--clusters)
           (paroliere-gg--linked-onset))
     '(6 1 2))))

(defun paroliere-gg--nucleus ()
  "Pick a syllable nucleus, mostly a plain vowel."
  (paroliere-gg--weighted-choice
   (list (paroliere-gg--vowel)
         (paroliere-gg--choice paroliere-gg--diphthongs))
   '(8 1)))

(defun paroliere-gg--syllable (&optional no-coda)
  "Build a syllable, passing NO-CODA on to `paroliere-gg--onset'.
Return a cons of the syllable text and whether its nucleus is a
diphthong, so that the next syllable can stay light after it.  A
diphthong is only used after a single consonant, never after a cluster."
  (let* ((onset (paroliere-gg--onset no-coda))
         (nucleus (if (= (length onset) 1)
                      (paroliere-gg--nucleus)
                    (paroliere-gg--vowel))))
    (cons (concat onset nucleus) (> (length nucleus) 1))))

(defun paroliere-gg--initial-syllable ()
  "Build an initial syllable, sometimes preceded by a bare vowel."
  (let* ((lead (paroliere-gg--weighted-choice
                (list "" (paroliere-gg--vowel)) '(6 1)))
         (syllable (paroliere-gg--syllable (string-empty-p lead))))
    (cons (concat lead (car syllable)) (cdr syllable))))

(defun paroliere-gg--final-syllable (&optional no-coda)
  "Build a final syllable, passing NO-CODA on to `paroliere-gg--onset'.
The word ends on a vowel, or on one of the few consonants Romance words
may close with when the last nucleus is a plain vowel."
  (let* ((onset (paroliere-gg--onset no-coda))
         (vowel (paroliere-gg--weighted-choice
                 paroliere-gg--final-vowels paroliere-gg--final-vowel-weights))
         (coda (if (= (length onset) 1)
                   (paroliere-gg--weighted-choice
                    (list "" (paroliere-gg--choice
                              paroliere-gg--final-consonants))
                    '(7 1))
                 "")))
    (concat onset vowel coda)))

(defun paroliere-gg-word ()
  "Return one made-up word of two to four syllables."
  (let* ((middle (paroliere-gg--weighted-choice '(0 1 2) '(3 4 2)))
         (syllable (paroliere-gg--initial-syllable))
         (word (car syllable))
         (heavy (cdr syllable)))
    (dotimes (_ middle)
      (setq syllable (paroliere-gg--syllable heavy)
            word (concat word (car syllable))
            heavy (cdr syllable)))
    (concat word (paroliere-gg--final-syllable heavy))))

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
