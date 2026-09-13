;;; scoundrel.el --- Scoundrel: A rogue-like card game -*- lexical-binding: t -*-

;; Copyright (C) 2026
;; Version: 1.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: games

;;; Commentary:

;; Scoundrel is a single-player rogue-like card game by Zach Gage and
;; Kurt Bieg.  Play with `M-x scoundrel'.
;;
;; Deck: standard 52 minus jokers, red face cards, and red aces (44 cards).
;; Clubs/Spades = Monsters, Diamonds = Weapons, Hearts = Potions.
;; Navigate rooms of 4 cards, taking 3 each turn with 20 HP.

;;; Code:

(require 'cl-lib)

(defgroup scoundrel nil
  "Scoundrel card game."
  :group 'games
  :prefix "scoundrel-")

;;; Faces

(defface scoundrel-monster
  '((((background dark)) :foreground "#cc6666" :weight bold)
    (t :foreground "#a00000" :weight bold))
  "Face for monster cards." :group 'scoundrel)

(defface scoundrel-weapon
  '((((background dark)) :foreground "#81a2be" :weight bold)
    (t :foreground "#2050a0" :weight bold))
  "Face for weapon cards." :group 'scoundrel)

(defface scoundrel-potion
  '((((background dark)) :foreground "#b5bd68" :weight bold)
    (t :foreground "#207020" :weight bold))
  "Face for potion cards." :group 'scoundrel)

(defface scoundrel-health
  '((((background dark)) :foreground "#b5bd68")
    (t :foreground "#207020"))
  "Face for health bar." :group 'scoundrel)

(defface scoundrel-damage
  '((((background dark)) :foreground "#cc6666")
    (t :foreground "#a00000"))
  "Face for lost health." :group 'scoundrel)

(defface scoundrel-key
  '((((background dark)) :foreground "#de935f")
    (t :foreground "#b05000"))
  "Face for key hints." :group 'scoundrel)

(defface scoundrel-dim
  '((((background dark)) :foreground "#707070")
    (t :foreground "#a0a0a0"))
  "Face for secondary text." :group 'scoundrel)

(defface scoundrel-title
  '((t :weight bold :height 1.2))
  "Face for title." :group 'scoundrel)

;;; Card representation — cons cell (VALUE . SUIT)
;; VALUE: 2-14 (J=11 Q=12 K=13 A=14)
;; SUIT: clubs, spades, diamonds, hearts

(defsubst scoundrel--card-value (card) (car card))
(defsubst scoundrel--card-suit (card) (cdr card))

(defun scoundrel--card-type (card)
  (pcase (cdr card)
    ((or 'clubs 'spades) 'monster)
    ('diamonds 'weapon)
    ('hearts 'potion)))

(defun scoundrel--value-label (value)
  (pcase value (11 "J") (12 "Q") (13 "K") (14 "A")
    (_ (number-to-string value))))

(defun scoundrel--suit-char (suit)
  (pcase suit ('clubs "♣") ('spades "♠") ('diamonds "♦") ('hearts "♥")))

(defun scoundrel--card-face (card)
  (pcase (scoundrel--card-type card)
    ('monster 'scoundrel-monster)
    ('weapon 'scoundrel-weapon)
    ('potion 'scoundrel-potion)))

(defun scoundrel--card-short (card)
  (format "%s%s" (scoundrel--value-label (car card))
          (scoundrel--suit-char (cdr card))))

(defun scoundrel--card-type-label (card)
  (pcase (scoundrel--card-type card)
    ('monster "Monster") ('weapon "Weapon") ('potion "Potion")))

;;; Card visual — 5 lines, 7 chars wide

(defun scoundrel--card-lines (card)
  "Return 5 propertized strings for CARD."
  (let* ((v (scoundrel--value-label (car card)))
         (s (scoundrel--suit-char (cdr card)))
         (face (scoundrel--card-face card))
         (vw (length v))
         (pad (- 5 vw)))
    (mapcar (lambda (str) (propertize str 'face face))
            (list "┌─────┐"
                  (format "│%s%s│" v (make-string pad ?\s))
                  (format "│  %s  │" s)
                  (format "│%s%s│" (make-string pad ?\s) v)
                  "└─────┘"))))

(defun scoundrel--insert-cards-row (card-lines-list)
  "Insert cards side by side from CARD-LINES-LIST."
  (dotimes (row 5)
    (insert "  ")
    (cl-loop for (cl . rest) on card-lines-list
             do (insert (nth row cl))
             when rest do (insert "  "))
    (insert "\n")))

;;; Deck

(defun scoundrel--make-deck ()
  "Build the 44-card Scoundrel deck."
  (let (deck)
    (dolist (suit '(clubs spades))
      (dotimes (i 13) (push (cons (+ i 2) suit) deck)))
    (dolist (suit '(diamonds hearts))
      (dotimes (i 9) (push (cons (+ i 2) suit) deck)))
    deck))

(defun scoundrel--shuffle (list)
  "Return shuffled copy of LIST via Fisher-Yates."
  (let* ((vec (vconcat list))
         (n (length vec)))
    (cl-loop for i from (1- n) downto 1
             for j = (random (1+ i))
             do (cl-rotatef (aref vec i) (aref vec j)))
    (append vec nil)))

;;; Game state

(defvar-local scoundrel--dungeon nil)
(defvar-local scoundrel--room nil)
(defvar-local scoundrel--health 20)
(defvar-local scoundrel--weapon nil)
(defvar-local scoundrel--weapon-kills nil "Monsters on weapon, most recent first.")
(defvar-local scoundrel--discard nil)
(defvar-local scoundrel--last-avoided nil)
(defvar-local scoundrel--potion-used nil)
(defvar-local scoundrel--cards-taken 0)
(defvar-local scoundrel--phase 'pick "One of: pick, combat, game-over.")
(defvar-local scoundrel--combat-monster nil)
(defvar-local scoundrel--message "")
(defvar-local scoundrel--score nil)
(defvar-local scoundrel--last-card nil)

;;; Game logic

(defun scoundrel--init ()
  "Start a new game."
  (setq scoundrel--dungeon (scoundrel--shuffle (scoundrel--make-deck))
        scoundrel--room nil
        scoundrel--health 20
        scoundrel--weapon nil
        scoundrel--weapon-kills nil
        scoundrel--discard nil
        scoundrel--last-avoided nil
        scoundrel--potion-used nil
        scoundrel--cards-taken 0
        scoundrel--phase 'pick
        scoundrel--combat-monster nil
        scoundrel--message "You descend into the dungeon..."
        scoundrel--score nil
        scoundrel--last-card nil)
  (scoundrel--start-turn))

(defun scoundrel--fill-room ()
  "Draw from dungeon until room has 4 cards."
  (while (and (< (length scoundrel--room) 4) scoundrel--dungeon)
    (setq scoundrel--room
          (append scoundrel--room (list (pop scoundrel--dungeon))))))

(defun scoundrel--start-turn ()
  "Begin a new turn: reset per-turn state, fill room."
  (setq scoundrel--potion-used nil
        scoundrel--cards-taken 0
        scoundrel--phase 'pick
        scoundrel--combat-monster nil)
  (scoundrel--fill-room)
  (if (null scoundrel--room)
      (scoundrel--game-over t)
    (scoundrel--render)))

(defun scoundrel--can-avoid-p ()
  (and (eq scoundrel--phase 'pick)
       (zerop scoundrel--cards-taken)
       (not scoundrel--last-avoided)
       scoundrel--room))

(defun scoundrel--avoid-room ()
  "Avoid the room, placing all cards at bottom of dungeon."
  (if (not (scoundrel--can-avoid-p))
      (progn
        (setq scoundrel--message "You can't avoid this room!")
        (scoundrel--render))
    (setq scoundrel--dungeon (append scoundrel--dungeon scoundrel--room)
          scoundrel--room nil
          scoundrel--last-avoided t
          scoundrel--message "You retreat and take another path...")
    (scoundrel--start-turn)))

(defun scoundrel--can-use-weapon-p (monster)
  "Can the equipped weapon be used against MONSTER?"
  (and scoundrel--weapon
       (or (null scoundrel--weapon-kills)
           (<= (car monster) (caar scoundrel--weapon-kills)))))

(defun scoundrel--pick-card (index)
  "Pick card at INDEX (0-based) from room."
  (cond
   ((not (eq scoundrel--phase 'pick))
    (setq scoundrel--message "Resolve combat first!")
    (scoundrel--render))
   ((>= index (length scoundrel--room))
    (setq scoundrel--message "No card at that position!")
    (scoundrel--render))
   (t
    (let ((card (nth index scoundrel--room)))
      (setq scoundrel--room (append (cl-subseq scoundrel--room 0 index)
                                    (cl-subseq scoundrel--room (1+ index)))
            scoundrel--last-card card)
      (cl-incf scoundrel--cards-taken)
      (pcase (scoundrel--card-type card)
        ('weapon (scoundrel--equip-weapon card))
        ('potion (scoundrel--use-potion card))
        ('monster
         (if (scoundrel--can-use-weapon-p card)
             (let ((wdmg (max 0 (- (car card) (car scoundrel--weapon))))
                   (bdmg (car card)))
               (setq scoundrel--phase 'combat
                     scoundrel--combat-monster card
                     scoundrel--message
                     (format "%s blocks your path!  [w] Weapon (%d dmg)  [b] Barehanded (%d dmg)"
                             (scoundrel--card-short card) wdmg bdmg))
               (scoundrel--render))
           (scoundrel--fight-barehanded card))))))))

(defun scoundrel--equip-weapon (card)
  "Equip CARD as weapon, discarding old weapon + kills."
  (when scoundrel--weapon
    (push scoundrel--weapon scoundrel--discard)
    (dolist (k scoundrel--weapon-kills)
      (push k scoundrel--discard)))
  (let ((old scoundrel--weapon))
    (setq scoundrel--weapon card
          scoundrel--weapon-kills nil
          scoundrel--message
          (if old
              (format "Equipped %s. Discarded %s."
                      (scoundrel--card-short card)
                      (scoundrel--card-short old))
            (format "Equipped %s." (scoundrel--card-short card)))))
  (scoundrel--check-turn-end))

(defun scoundrel--use-potion (card)
  "Use health potion CARD."
  (if scoundrel--potion-used
      (progn
        (push card scoundrel--discard)
        (setq scoundrel--message
              (format "%s wasted — only one potion per turn."
                      (scoundrel--card-short card))))
    (let* ((heal (car card))
           (old-hp scoundrel--health)
           (new-hp (min 20 (+ old-hp heal)))
           (actual (- new-hp old-hp)))
      (setq scoundrel--health new-hp
            scoundrel--potion-used t)
      (push card scoundrel--discard)
      (setq scoundrel--message
            (if (> actual 0)
                (format "%s restored %d HP. (%d → %d)"
                        (scoundrel--card-short card) actual old-hp new-hp)
              (format "%s — already at full health."
                      (scoundrel--card-short card))))))
  (scoundrel--check-turn-end))

(defun scoundrel--fight-barehanded (monster)
  "Fight MONSTER barehanded: full damage."
  (let* ((dmg (car monster))
         (old-hp scoundrel--health))
    (cl-decf scoundrel--health dmg)
    (push monster scoundrel--discard)
    (setq scoundrel--phase 'pick
          scoundrel--combat-monster nil
          scoundrel--message
          (format "Fought %s barehanded! %d damage. (%d → %d)"
                  (scoundrel--card-short monster) dmg old-hp scoundrel--health))
    (if (<= scoundrel--health 0)
        (scoundrel--game-over nil)
      (scoundrel--check-turn-end))))

(defun scoundrel--fight-with-weapon (monster)
  "Fight MONSTER with equipped weapon."
  (let* ((mv (car monster))
         (wv (car scoundrel--weapon))
         (dmg (max 0 (- mv wv)))
         (old-hp scoundrel--health))
    (cl-decf scoundrel--health dmg)
    (push monster scoundrel--weapon-kills)
    (setq scoundrel--phase 'pick
          scoundrel--combat-monster nil
          scoundrel--message
          (if (zerop dmg)
              (format "Slew %s with %s — no damage!"
                      (scoundrel--card-short monster)
                      (scoundrel--card-short scoundrel--weapon))
            (format "Fought %s with %s. %d damage. (%d → %d)"
                    (scoundrel--card-short monster)
                    (scoundrel--card-short scoundrel--weapon)
                    dmg old-hp scoundrel--health)))
    (if (<= scoundrel--health 0)
        (scoundrel--game-over nil)
      (scoundrel--check-turn-end))))

(defun scoundrel--check-turn-end ()
  "End turn if 3 cards taken or room nearly empty."
  (cond
   ((>= scoundrel--cards-taken 3)
    (setq scoundrel--last-avoided nil)
    (scoundrel--start-turn))
   ((<= (length scoundrel--room) 1)
    (if (and (null scoundrel--room) (null scoundrel--dungeon))
        (scoundrel--game-over t)
      (setq scoundrel--last-avoided nil)
      (scoundrel--start-turn)))
   (t (scoundrel--render))))

(defun scoundrel--game-over (won)
  "End the game. WON non-nil means player survived."
  (setq scoundrel--phase 'game-over)
  (if won
      (let ((score scoundrel--health))
        (when (and (= scoundrel--health 20)
                   scoundrel--last-card
                   (eq (scoundrel--card-type scoundrel--last-card) 'potion))
          (cl-incf score (car scoundrel--last-card)))
        (setq scoundrel--score score
              scoundrel--message
              (format "You escaped the dungeon! Final score: %d" score)))
    (let ((penalty 0))
      (dolist (card (append scoundrel--room scoundrel--dungeon))
        (when (eq (scoundrel--card-type card) 'monster)
          (cl-incf penalty (car card))))
      (setq scoundrel--score (- scoundrel--health penalty)
            scoundrel--message
            (format "You perished in the dungeon... Score: %d"
                    scoundrel--score))))
  (scoundrel--render))

;;; Rendering

(defun scoundrel--render ()
  "Redraw the game buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (scoundrel--render-title)
    (scoundrel--render-health)
    (scoundrel--render-dungeon-info)
    (insert "\n")
    (scoundrel--render-room)
    (insert "\n")
    (scoundrel--render-weapon)
    (insert "\n")
    (scoundrel--render-message)
    (insert "\n")
    (scoundrel--render-actions)
    (goto-char (point-min))))

(defun scoundrel--render-title ()
  (insert (propertize "  ⚔ SCOUNDREL ⚔\n" 'face 'scoundrel-title))
  (insert (propertize "  ═════════════════════════════════════\n"
                      'face 'scoundrel-dim)))

(defun scoundrel--render-health ()
  (let* ((hp (max 0 scoundrel--health))
         (filled (min 20 hp))
         (empty (- 20 filled)))
    (insert "  HP "
            (propertize (make-string filled ?█) 'face 'scoundrel-health)
            (propertize (make-string empty ?░) 'face 'scoundrel-damage)
            (format " %d/20\n" scoundrel--health))))

(defun scoundrel--render-dungeon-info ()
  (insert (propertize (format "  Dungeon: %d cards remaining\n"
                              (length scoundrel--dungeon))
                      'face 'scoundrel-dim)))

(defun scoundrel--render-room ()
  (insert (propertize "  ── Room ──────────────────────────\n"
                      'face 'scoundrel-dim))
  (if (null scoundrel--room)
      (insert (propertize "  (empty)\n" 'face 'scoundrel-dim))
    ;; Index labels
    (insert "  ")
    (dotimes (i (length scoundrel--room))
      (let ((label (format "  [%d]  " (1+ i))))
        (insert (propertize label 'face 'scoundrel-key))
        (when (< i (1- (length scoundrel--room)))
          (insert "  "))))
    (insert "\n")
    ;; Cards
    (scoundrel--insert-cards-row
     (mapcar #'scoundrel--card-lines scoundrel--room))
    ;; Type labels
    (insert "  ")
    (cl-loop for (card . rest) on scoundrel--room
             for label = (scoundrel--card-type-label card)
             for face = (scoundrel--card-face card)
             for pad-l = (/ (- 7 (length label)) 2)
             for pad-r = (- 7 (+ pad-l (length label)))
             do (insert (make-string pad-l ?\s)
                        (propertize label 'face face)
                        (make-string pad-r ?\s))
             when rest do (insert "  "))
    (insert "\n")))

(defun scoundrel--render-weapon ()
  (insert (propertize "  ── Weapon ────────────────────────\n"
                      'face 'scoundrel-dim))
  (if (null scoundrel--weapon)
      (insert (propertize "  None equipped (fighting barehanded)\n"
                          'face 'scoundrel-dim))
    (scoundrel--insert-cards-row
     (list (scoundrel--card-lines scoundrel--weapon)))
    (when scoundrel--weapon-kills
      (insert "  Kills: ")
      (cl-loop for (k . rest) on (reverse scoundrel--weapon-kills)
               do (insert (propertize (scoundrel--card-short k)
                                      'face 'scoundrel-monster)
                          (format " (%d)" (car k)))
               when rest do (insert " → "))
      (insert "\n")
      (insert (propertize
               (format "  Can slay monsters with value ≤ %d\n"
                       (caar scoundrel--weapon-kills))
               'face 'scoundrel-dim)))))

(defun scoundrel--render-message ()
  (when (and scoundrel--message (not (string-empty-p scoundrel--message)))
    (if (eq scoundrel--phase 'game-over)
        (insert "\n  " (propertize scoundrel--message 'face 'bold) "\n")
      (insert "  " scoundrel--message "\n"))))

(defun scoundrel--render-actions ()
  (insert (propertize "  ── Actions ───────────────────────\n"
                      'face 'scoundrel-dim))
  (pcase scoundrel--phase
    ('pick
     (insert "  "
             (propertize "[1-4]" 'face 'scoundrel-key) " Take card")
     (when (scoundrel--can-avoid-p)
       (insert "  "
               (propertize "[a]" 'face 'scoundrel-key) " Avoid room"))
     (insert "  "
             (propertize "[n]" 'face 'scoundrel-key) " New game"
             "  "
             (propertize "[q]" 'face 'scoundrel-key) " Quit\n")
     (when (> scoundrel--cards-taken 0)
       (let ((target (min 3 (max 1 (1- (+ scoundrel--cards-taken
                                           (length scoundrel--room)))))))
         (insert (propertize
                  (format "  Cards taken: %d/%d\n"
                          scoundrel--cards-taken target)
                  'face 'scoundrel-dim)))))
    ('combat
     (insert "  "
             (propertize "[w]" 'face 'scoundrel-key) " Fight with weapon"
             "  "
             (propertize "[b]" 'face 'scoundrel-key) " Fight barehanded"
             "\n"))
    ('game-over
     (insert "  "
             (propertize "[n]" 'face 'scoundrel-key) " New game"
             "  "
             (propertize "[q]" 'face 'scoundrel-key) " Quit\n"))))

;;; Interactive commands

(defun scoundrel-pick-1 () (interactive) (scoundrel--pick-card 0))
(defun scoundrel-pick-2 () (interactive) (scoundrel--pick-card 1))
(defun scoundrel-pick-3 () (interactive) (scoundrel--pick-card 2))
(defun scoundrel-pick-4 () (interactive) (scoundrel--pick-card 3))

(defun scoundrel-avoid ()
  "Avoid the current room."
  (interactive)
  (scoundrel--avoid-room))

(defun scoundrel-weapon ()
  "Fight the monster with your weapon."
  (interactive)
  (if (eq scoundrel--phase 'combat)
      (scoundrel--fight-with-weapon scoundrel--combat-monster)
    (message "Not in combat!")))

(defun scoundrel-barehanded ()
  "Fight the monster barehanded."
  (interactive)
  (if (eq scoundrel--phase 'combat)
      (scoundrel--fight-barehanded scoundrel--combat-monster)
    (message "Not in combat!")))

(defun scoundrel-new-game ()
  "Start a new game of Scoundrel."
  (interactive)
  (when (or (eq scoundrel--phase 'game-over)
            (y-or-n-p "Abandon current game? "))
    (scoundrel--init)))

(defun scoundrel-quit ()
  "Quit Scoundrel."
  (interactive)
  (when (or (eq scoundrel--phase 'game-over)
            (y-or-n-p "Quit Scoundrel? "))
    (quit-window t)))

;;; Mode

(defvar scoundrel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "1") #'scoundrel-pick-1)
    (define-key map (kbd "2") #'scoundrel-pick-2)
    (define-key map (kbd "3") #'scoundrel-pick-3)
    (define-key map (kbd "4") #'scoundrel-pick-4)
    (define-key map (kbd "a") #'scoundrel-avoid)
    (define-key map (kbd "w") #'scoundrel-weapon)
    (define-key map (kbd "b") #'scoundrel-barehanded)
    (define-key map (kbd "n") #'scoundrel-new-game)
    (define-key map (kbd "q") #'scoundrel-quit)
    map)
  "Keymap for Scoundrel mode.")

(define-derived-mode scoundrel-mode special-mode "Scoundrel"
  "Major mode for playing Scoundrel.

\\{scoundrel-mode-map}"
  (setq buffer-read-only t
        truncate-lines t))

;;;###autoload
(defun scoundrel ()
  "Play Scoundrel, a single-player rogue-like card game."
  (interactive)
  (switch-to-buffer (get-buffer-create "*Scoundrel*"))
  (scoundrel-mode)
  (scoundrel--init))

(provide 'scoundrel)
;;; scoundrel.el ends here
