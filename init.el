;;;  -*- lexical-binding: t; -*-

;;; PACKAGE CEREMONY

(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))

(require 'use-package)

(add-to-list 'load-path
  (expand-file-name "gg-lisp" user-emacs-directory))


;;; PACKAGES OF MINE

(use-package gg-utils
  :demand t)

(use-package gg-sclang-aux)

(use-package gg-xkcd-geohashing
  :defer t
  :commands (gg/xkcd-geohashing-coords
	     gg/browse-xkcd-geohashing-map))

(use-package gg-serendip
  :defer t
  :commands (gg/serendip-transient))

(use-package drench
  :defer t
  :commands (drench))

(use-package scoundrel
  :defer t
  :commands (scoundrel))

(use-package preshell
  :custom
  (preshell-commands-alist
   '(("pacman update" . "sudo pacman -Syu")
     ("pacman list installed" . "pacman -Q")
     ("pacman list installed expliclitly" . "pacman -Qe")
     ("pacman install from package list" . "sudo pacman --needed -S - < /home/gg/.config/pkgs/arch")
     ("nix env update" . "nix-channel --update && nix-env -u '*'")
     ("nix env list installed" . "nix-env --query")
     ("nix garbage collect" . "nix-collect-garbage")
     ("restow emacs" . "cd /home/gg/box/prj/2016/conchiglie/home && stow -R emacs")
     ("ollama serve" . "ollama serve"))))

(use-package dired-prefab
  :defer t
  :commands (dired-prefab)
  :bind (:map dired-mode-map ("," . dired-prefab))
  :custom
  (dired-prefab-single-commands
   '((:name "Play video"
      :command "mpv %{}"
      :extensions video)
     (:name "Open with xdg-open"
      :command "xdg-open %{}"
      :type any)
     (:name "Convert image"
      :command "convert %{} -resize %{Scale:50%|75%|100%|150%}% %{file}"
      :extensions images)
     (:name "Unpack archive"
      :command "tar xf %{} -C %{existing-dir}"
      :extensions archives)
     (:name "Convert audio to mp3"
      :command "ffmpeg -i %{} %{.mp3}"
      :extensions audio)))
  (dired-prefab-multi-commands
   '((:name "Join PDFs"
      :command "gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=%{file} %{}"
      :extensions ("pdf"))
     (:name "Concatenate audio"
      :command "bash -c \"printf 'file %s\\n' %{} | ffmpeg -f concat -safe 0 -i - -c copy %{file}\""
      :extensions audio)
     (:name "Join images vertically"
      :command "magick %{} -append %{file}"
      :extensions images)
     (:name "Join images horizontally"
      :command "magick %{} +append %{file}"
      :extensions images)
     (:name "Images to PDF"
      :command "magick %{} %{file}"
      :extensions images)
     (:name "Concatenate video"
      :command "bash -c \"printf 'file %s\\n' %{} | ffmpeg -f concat -safe 0 -i - -c copy %{file}\""
      :extensions video))))

(use-package gg-insert-date
  :custom
  (gg/insert-date-formats '("%F" "%FT%T%:z" "%YW%V"))
  :bind
  ("C-c d" . gg/insert-current-date))

(use-package gg-inc-at-point
  :bind
  ("C-c +" . gg/increment-number-at-point))

(use-package gg-workspace
  :bind
  ("<f5>" . gg/apparecchia))

(use-package gg-org-yt
  :defer t
  :commands (gg/yt-playlist-to-org))

(use-package gg-pirate
  :defer t
  :commands (gg/movie-pirate))

(use-package gg-other-win
  :bind ("C-x 4 k" . gg/kill-buffer-and-windows-other-window))

(use-package gg-align-columns
  :defer t
  :commands (gg/align-columns-by-whitespace))

(use-package gg-isearch-pop-yank
  :bind
  (:map isearch-mode-map
        ("C-c p" . isearch-mark-whole-paragraph-pop-mark-and-yank)))

(use-package progress-mode
  :defer t
  :commands (progress-day progress-week progress-month progress-year))

(use-package gg-unicode
  :config
  (with-eval-after-load 'iso-transl
    (gg/more-ctl-x-8-superscript)
    (gg/more-ctl-x-8-subscript)
    (gg/more-ctl-x-8-greek)))

(use-package gg-websearch
  :defer t
  :commands (gg-websearch-dwim))

(use-package figlet
  :defer t
  :commands (figlet-insert figlet-render figlet-font-list)
  :custom (figlet-font-directory '(detect "/usr/share/figlet")))

(use-package mark-column-rectangle
  :defer t
  :commands (mark-column-rectangle))

(use-package gg-mark-whole
  :bind
  (("C-c m w" . gg-mark-whole-word)
   ("C-c m W" . gg-mark-whole-WORD)
   ("C-c m l" . gg-mark-whole-line)
   ("C-c m L" . gg-mark-whole-line-with-newline)
   ("C-c m p" . gg-mark-whole-paragraph)
   ("C-c m d" . gg-mark-whole-function)
   ("C-c m b" . gg-mark-whole-buffer)
   ("C-c m n" . gg-mark-whole-number)
   ("C-c m e" . gg-mark-whole-email)
   ("C-c m f" . gg-mark-whole-filename)
   ("C-c m s" . gg-mark-whole-whitespace)
   ("C-c m u" . gg-mark-whole-url)
   ("C-c m m" . gg-mark-whole-dwim)
   ("C-c m r" . mark-column-rectangle)))

(use-package gg-epoch
  :defer t
  :commands (gg/epoch-to-timestamp))

(use-package gg-rot128
  :defer t
  :commands (gg/rot128-file))

(use-package watch
  :defer t
  :commands (watch-start))

(use-package gg-tidal-aux
  :defer t
  :after tidal
  :commands (gg/tidal-setup))

(use-package gg-erlang-aux
  :defer t
  :commands (gg/require-erlang-from-distro))


;;; BUILT-IN PACKAGES CONFIG

(use-package emacs
  :bind
  (("C-x k" . kill-current-buffer)
   ("C-c w" . visual-line-mode)
   ("M-=" . count-words)))  ; counts region or buffer

(use-package icomplete
  :demand t
  :custom
  (icomplete-show-matches-on-no-input t)
  (completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  :config
  (icomplete-vertical-mode 1)
  :bind
  (:map icomplete-vertical-mode-minibuffer-map
        ("<TAB>" . icomplete-force-complete)))

(use-package org
  :defer t
  :custom
  (org-ellipsis "⤵")
  (org-startup-folded t)
  (org-tags-column 0)
  (org-clock-sound "~/dl/audio/alarm.wav")
  (org-todo-keywords
	'((sequence "TODO" "ASIDE" "DONE")))
  (org-todo-keyword-faces
	'(("ASIDE" . "dark blue")))
  :custom-face
  (org-ellipsis ((t (:underline nil))))
  :hook
  (org-mode . (lambda ()
		"disable line numbers in org-mode, too distracting"
		(display-line-numbers-mode 0)
		(electric-indent-mode -1))))

(use-package cc-mode
  :hook
  (c++-mode .
	    (lambda ()
              (setq-local
	       tab-width 4
               c-basic-offset 4
               backward-delete-char-untabify-method nil))))


;;; EXTERNAL PACKAGES

(use-package ledger-mode
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t
  :bind ("C-x g" . magit-status))

(use-package evil
  :ensure t
  :bind ("C-c v" . evil-local-mode))

(use-package move-text
  :ensure t
  :config (move-text-default-bindings))

(use-package sclang
  :defer t
  :after (gg-sclang-aux)
  :commands (sclang-start)
  :custom
  (sclang-show-workspace-on-startup nil)
  (sclang-eval-line-forward nil)
  :mode ("\\.scd\\'" . sclang-mode)
  :bind (:map sclang-mode-map
	      ("C-<return>" . gg/sclang-eval-dwim)
	      ("C-." . sclang-main-stop)
	      ("C-c t" . gg/sclang-transient)))

(use-package ox-hugo
  :ensure t
  :pin melpa
  :after ox)


;;; AUTOSAVE AND BACKUP

;; put autosave and backup files inside .emacs.d/ subdirs

(setq
 my-autosave-dir (expand-file-name "autosaves" user-emacs-directory)
 my-backup-dir (expand-file-name "backup" user-emacs-directory))

(make-directory my-autosave-dir :parents)
(make-directory my-backup-dir :parents)

(setq
 auto-save-file-name-transforms `((".*" ,my-autosave-dir t))
 backup-directory-alist `((".*". ,my-backup-dir)))

;; put emacs-customized values in a separate file instead of appending it to
;; init.el but never load it, I want init.el to the only source of truth

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))


;;; UX AND GFX

;; random theme
(let ((theme (seq-random-elt (custom-available-themes))))
  (load-theme theme t)
  (message "Loaded random theme: %s" theme))

;; type y instead of yes
(setq use-short-answers t)

;; set fill-column (alt-q) to 80 char per lines
(setq-default fill-column 80)

;; show matching parens
(show-paren-mode 1)

;; middle mouse click yank at text cursor
;; not al mouse cursor position
(setq mouse-yank-at-point t)

;; re-enable 'a' in dired
(put 'dired-find-alternate-file 'disabled nil)

;; use loopback mode in gpg (epa) pin-entry
;; so it's emacs that asks the password
(setq epa-pinentry-mode 'loopback)

;; launch http url in chromium incognito
(setq browse-url-browser-function
      (quote browse-url-generic))
(setq browse-url-generic-args
      (quote ("--private-window")))
(setq browse-url-generic-program
      "firefox")

;; electric pair auto-inserts matching parens and quotes
(electric-pair-mode 1)

;; set default font and size
(set-face-attribute
 'default nil :family "Source Code Pro" :height 130)

;; visible bell
(setq visible-bell t)

;; show line number on the side
(global-display-line-numbers-mode)

;; column number in info bar
(column-number-mode 1)

;; *scratch* default text
(setq initial-scratch-message
      (concat (replace-regexp-in-string "^" ";; " (figlet-render "standard" "scratch")) "\n\n"))


;; on my new Lenovo E14 gen5 Fn key is interpreded as <WakeUp> so we ignore it
(when (gg/is-computer-model? "ThinkPad E14 Gen 5")
  (global-set-key (kbd "<WakeUp>") 'ignore))
