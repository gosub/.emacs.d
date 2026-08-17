;;; grammelot-gg.el --- Generate made-up Romance and Germanic words -*- lexical-binding: t; -*-

;; Named after the grammelot of the commedia dell'arte, the gibberish
;; that sounds like a real language without being one.
;;
;; A style is a plist describing the phonotactics of a family of
;; languages: which onsets, nuclei and codas exist, which consonant pairs
;; may meet across a syllable boundary, and how a word may end.  Two are
;; provided, Romance and Germanic, and each word picks one at random, so
;; a batch reads like a phrasebook for two neighbouring invented tongues.

(defvar grammelot-gg-buffer-name "*grammelot*"
  "Name of the buffer `grammelot-gg' fills with words.")


;;; Styles

(defconst grammelot-gg-romance
  '(:name "romance"
    ;; Italian, Latin and Spanish flavoured: light syllables, and words
    ;; that almost always end on a vowel.
    :syllable-counts (2 3 4)
    :syllable-weights (3 4 2)
    :lead-vowel-weights (6 1)
    :vowels ("a" "e" "i" "o" "u")
    :vowel-weights (5 4 2 4 1)
    :diphthongs ("ia" "ie" "io" "iu" "ua" "ue" "ui" "uo" "ai" "au" "ei" "oi")
    :diphthong-weights (8 1)
    :consonants ("b" "c" "d" "f" "g" "l" "m" "n" "p" "r" "s" "t" "v" "z")
    :consonant-weights (2 4 4 2 2 4 4 4 3 4 3 4 2 1)
    ;; Muta cum liquida, the only onset clusters these languages allow.
    :clusters ("br" "cr" "dr" "fr" "gr" "pr" "tr"
               "bl" "cl" "fl" "gl" "pl")
    :initial-onset-weights (7 1)
    :onset-weights (6 1 2)
    :geminable ("b" "c" "d" "f" "g" "l" "m" "n" "p" "r" "s" "t" "z")
    ;; Note the split between N and M: Romance spelling writes M, not N,
    ;; before B and P.
    :coda-onsets (("n" "c" "d" "f" "g" "s" "t" "v" "z")
                  ("m" "b" "p")
                  ("r" "b" "c" "d" "f" "g" "l" "m" "n" "p" "s" "t" "v" "z")
                  ("s" "c" "p" "t")
                  ("l" "b" "c" "d" "g" "m" "p" "t" "v" "z"))
    :heavy-syllables nil
    :final-vowels ("a" "e" "i" "o")
    :final-vowel-weights (6 4 1 6)
    :open-endings ("ia" "ie" "io" "ua" "ue" "uo" "ai" "ei" "oi")
    :open-ending-weights (9 1)
    :final-codas ("n" "s" "r" "l")
    :final-coda-clusters ()
    :final-coda-weights (7 1 0))
  "Phonotactics of a Romance sounding language.")

(defconst grammelot-gg-germanic
  '(:name "germanic"
    ;; English and its cousins: short words, rich onsets, and syllables
    ;; heavy enough to end on a consonant cluster.
    :syllable-counts (1 2 3)
    :syllable-weights (3 5 2)
    :lead-vowel-weights (6 1)
    :vowels ("a" "e" "i" "o" "u")
    :vowel-weights (4 5 4 3 2)
    :diphthongs ("ea" "ee" "oo" "ou" "ai" "oa" "au" "ei" "ie" "oi")
    :diphthong-weights (4 1)
    :consonants ("b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n"
                 "p" "r" "s" "t" "v" "w" "y" "th" "sh" "ch" "wh")
    :consonant-weights (3 3 4 3 3 4 1 3 4 4 4
                        3 4 5 5 1 3 1 3 2 2 1)
    :clusters ("bl" "br" "cl" "cr" "dr" "fl" "fr" "gl" "gr" "pl" "pr" "tr"
               "sc" "sk" "sl" "sm" "sn" "sp" "st" "sw" "tw"
               "str" "spr" "scr" "thr" "shr" "wr" "kn" "gn" "qu")
    :initial-onset-weights (5 2)
    :onset-weights (6 1 2)
    :geminable ("b" "d" "f" "g" "l" "m" "n" "p" "r" "s" "t" "z")
    ;; C before K spells the English CK, G before H spells GH.
    :coda-onsets (("n" "d" "t" "c" "g" "k" "s" "th")
                  ("r" "b" "c" "d" "f" "g" "k" "l" "m" "n" "p" "s" "t" "th" "v")
                  ("s" "c" "k" "l" "m" "n" "p" "t" "w")
                  ("l" "b" "c" "d" "f" "g" "k" "m" "p" "s" "t" "th" "v")
                  ("m" "b" "p" "f")
                  ("c" "k")
                  ("f" "t"))
    ;; The licence English takes and Romance does not: one syllable may
    ;; carry a cluster, a diphthong and a coda at once, as STRAIND does.
    :heavy-syllables t
    :final-vowels ("a" "e" "i" "o" "u")
    :final-vowel-weights (3 6 2 3 1)
    ;; An English word that stops on its vowel needs a showy one to stop
    ;; on: DRY and DROW read as words, DRE does not.  IGH lives here
    ;; rather than among the diphthongs, where it would draw codas it
    ;; never takes.
    :open-endings ("y" "ay" "ow" "aw" "ew" "oy" "ee" "ea" "oo" "igh")
    :open-ending-weights (1 4)
    :final-codas ("b" "d" "f" "g" "k" "l" "m" "n" "p" "r" "s" "t"
                  "th" "sh" "ch" "ck" "ng")
    :final-coda-clusters ("nd" "nt" "nk" "st" "sk" "sp" "lt" "ld" "lf" "lk"
                          "lm" "lp" "rd" "rk" "rl" "rm" "rn" "rp" "rt" "rth"
                          "ft" "mp" "nch")
    :final-coda-weights (3 4 3))
  "Phonotactics of a Germanic sounding language.")

(defvar grammelot-gg-styles
  (list grammelot-gg-romance grammelot-gg-germanic)
  "Styles `grammelot-gg--word' draws from, one at random per word.
Narrow this list to a single style to generate words of one kind only.")

(defvar grammelot-gg--style nil
  "The style plist in force while a word is being built.")

(defun grammelot-gg--get (key)
  "Return KEY from the style currently being spoken."
  (plist-get grammelot-gg--style key))


;;; Random helpers

(defun grammelot-gg--weighted-choice (population weights)
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

(defun grammelot-gg--choice (population)
  "Pick one element of POPULATION uniformly at random, or \"\" if it is empty."
  (if population
      (nth (random (length population)) population)
    ""))


;;; Syllable parts

(defun grammelot-gg--vowel ()
  "Pick a single vowel."
  (grammelot-gg--weighted-choice (grammelot-gg--get :vowels)
                                 (grammelot-gg--get :vowel-weights)))

(defun grammelot-gg--plain-p (nucleus)
  "Return non-nil if NUCLEUS is a single vowel rather than a diphthong."
  (member nucleus (grammelot-gg--get :vowels)))

(defun grammelot-gg--nucleus ()
  "Pick a syllable nucleus, mostly a plain vowel."
  (grammelot-gg--weighted-choice
   (list (grammelot-gg--vowel)
         (grammelot-gg--choice (grammelot-gg--get :diphthongs)))
   (grammelot-gg--get :diphthong-weights)))

(defun grammelot-gg--final-vowel ()
  "Pick a single vowel a word may end its last syllable on.
Not every vowel will do: no Italian word ends in U."
  (grammelot-gg--weighted-choice (grammelot-gg--get :final-vowels)
                                 (grammelot-gg--get :final-vowel-weights)))

(defun grammelot-gg--open-nucleus ()
  "Pick the nucleus of a word that ends on its vowel.
This is the last thing heard, so it is where a style keeps its showiest
endings: the Italian -IA, the English -Y."
  (grammelot-gg--weighted-choice
   (list (grammelot-gg--final-vowel)
         (grammelot-gg--choice (grammelot-gg--get :open-endings)))
   (grammelot-gg--get :open-ending-weights)))

(defun grammelot-gg--simple-p (onset)
  "Return non-nil if ONSET is one consonant of the current style.
Digraphs such as TH and SH count as one, clusters and codas do not."
  (member onset (grammelot-gg--get :consonants)))

(defun grammelot-gg--simple-onset ()
  "Pick a single consonant."
  (grammelot-gg--weighted-choice (grammelot-gg--get :consonants)
                                 (grammelot-gg--get :consonant-weights)))

(defun grammelot-gg--linked-onset ()
  "Pick an onset preceded by a coda, either a geminate or a legal pair."
  (let ((geminate (grammelot-gg--choice (grammelot-gg--get :geminable)))
        (pair (grammelot-gg--choice (grammelot-gg--get :coda-onsets))))
    (grammelot-gg--weighted-choice
     (list (concat geminate geminate)
           (concat (car pair) (grammelot-gg--choice (cdr pair))))
     '(1 1))))

(defun grammelot-gg--onset (&optional no-coda)
  "Pick a syllable onset, mostly a single consonant.
With NO-CODA never pick an onset carrying a coda on its left, as is
required word initially and after a diphthong."
  (if no-coda
      (grammelot-gg--weighted-choice
       (list (grammelot-gg--simple-onset)
             (grammelot-gg--choice (grammelot-gg--get :clusters)))
       (grammelot-gg--get :initial-onset-weights))
    (grammelot-gg--weighted-choice
     (list (grammelot-gg--simple-onset)
           (grammelot-gg--choice (grammelot-gg--get :clusters))
           (grammelot-gg--linked-onset))
     (grammelot-gg--get :onset-weights))))

(defun grammelot-gg--final-coda ()
  "Pick what closes a word: nothing, one consonant, or a cluster."
  (grammelot-gg--weighted-choice
   (list ""
         (grammelot-gg--choice (grammelot-gg--get :final-codas))
         (grammelot-gg--choice (grammelot-gg--get :final-coda-clusters)))
   (grammelot-gg--get :final-coda-weights)))


;;; Syllables

(defun grammelot-gg--syllable (&optional no-coda)
  "Build a syllable, passing NO-CODA on to `grammelot-gg--onset'.
Return a cons of the syllable text and whether the next syllable must
stay light, so that heavy syllables do not pile up."
  (let* ((onset (grammelot-gg--onset no-coda))
         (nucleus (if (or (grammelot-gg--simple-p onset)
                          (grammelot-gg--get :heavy-syllables))
                      (grammelot-gg--nucleus)
                    (grammelot-gg--vowel))))
    (cons (concat onset nucleus)
          (not (grammelot-gg--plain-p nucleus)))))

(defun grammelot-gg--final-syllable (&optional no-coda)
  "Build the last syllable, passing NO-CODA on to `grammelot-gg--onset'.
The coda is settled first, because a word ending on its vowel may choose
that vowel from the style's open endings."
  (let* ((onset (grammelot-gg--onset no-coda))
         (heavy (grammelot-gg--get :heavy-syllables))
         (room (or heavy (grammelot-gg--simple-p onset)))
         (coda (if room (grammelot-gg--final-coda) ""))
         (nucleus (cond ((not room) (grammelot-gg--final-vowel))
                        ((string-empty-p coda) (grammelot-gg--open-nucleus))
                        (heavy (grammelot-gg--nucleus))
                        (t (grammelot-gg--final-vowel)))))
    (concat onset nucleus coda)))


;;; Words

(defun grammelot-gg--word (&optional style)
  "Return one made-up word, upcased.
STYLE is a plist as in `grammelot-gg-styles', by default one of them
picked at random."
  (let* ((grammelot-gg--style (or style (grammelot-gg--choice
                                         grammelot-gg-styles)))
         (count (grammelot-gg--weighted-choice
                 (grammelot-gg--get :syllable-counts)
                 (grammelot-gg--get :syllable-weights)))
         (lead (grammelot-gg--weighted-choice
                (list "" (grammelot-gg--vowel))
                (grammelot-gg--get :lead-vowel-weights)))
         ;; A word may not open on a coda, but a leading vowel gives one
         ;; something to lean on, as in the Italian ANTICO.
         (no-coda (string-empty-p lead))
         (word lead))
    (dotimes (_ (1- count))
      (let ((syllable (grammelot-gg--syllable no-coda)))
        (setq word (concat word (car syllable))
              no-coda (cdr syllable))))
    (upcase (concat word (grammelot-gg--final-syllable no-coda)))))


;;; Commands

(defun grammelot-gg (&optional count)
  "Show COUNT made-up words, 7 by default, in the `*grammelot*' buffer.
Interactively, a numeric prefix argument sets COUNT.  Each call replaces
the previous contents, so repeating the command deals a fresh hand."
  (interactive "P")
  (let ((n (if count (prefix-numeric-value count) 7))
        (buffer (get-buffer-create grammelot-gg-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (dotimes (_ n)
        (insert (grammelot-gg--word) "\n"))
      (goto-char (point-min)))
    (pop-to-buffer buffer)))

(defun grammelot-word-gg ()
  "Show one made-up word in the echo area and put it on the kill ring."
  (interactive)
  (let ((word (grammelot-gg--word)))
    (kill-new word)
    (message "%s" word)
    word))

(provide 'grammelot-gg)
;;; grammelot-gg.el ends here
