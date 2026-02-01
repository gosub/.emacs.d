;;;  -*- lexical-binding: t; -*-

;;; PACKAGE CEREMONY

(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))

(require 'use-package)

(add-to-list 'load-path
  (expand-file-name "gg-lisp" user-emacs-directory))


;;; PACKAGES OF MINE

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

(use-package gg-insert-date
  :custom
  (gg/insert-date-format-list '("%F" "%FT%T%:z" "%YW%V"))
  :bind
  ("C-c d" . gg/insert-current-date))


;;; BUILT-IN PACKAGES CONFIG

(use-package icomplete
  :custom
  (icomplete-show-matches-on-no-input t)
  :config
  (icomplete-vertical-mode 1))

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
  :ensure t)

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


;;; Dirs and files

;; put autosave and backup files inside .emacs.d/ subdirs

(setq
 my-autosave-dir (expand-file-name "autosaves" user-emacs-directory)
 my-backup-dir (expand-file-name "backup" user-emacs-directory))

(make-directory my-autosave-dir :parents)
(make-directory my-backup-dir :parents)

(setq
 auto-save-file-name-transforms `((".*" ,(expand-file-name "\\1" my-autosave-dir) t))
 backup-directory-alist `((".*". ,my-backup-dir)))

;; put emacs-customized values in a separate file
;; instead of appending it to init.el
;; necessary since package-list and package-install
;; use it as record of explicitly installed packages

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))

;; I do not want to use customized values for configuration
;; init.el is the only source of truth

; (when (file-exists-p custom-file)
;   (load custom-file))


; most used files directory
(setq gg-txt-directory "~/box/txt")

; notes file
(setq gg-notes-file
      (expand-file-name "ziba.org" gg-txt-directory))
; todo file
(setq gg-todo-file
      (expand-file-name "todo.org" gg-txt-directory))
; done file
(setq gg-done-file
      (expand-file-name "done.org" gg-txt-directory))


;;; Functions


(defun gg/increment-number-at-point ()
  "increment number at point, partially simulating C-a in vim"
  (interactive)
  (save-excursion
    (skip-chars-backward "0123456789")
    (or (looking-at "[0123456789]+")
	(error "No number at point"))
    (replace-match (number-to-string 
                      (1+ (string-to-number (match-string 0)))))))


(defun gg/apparecchia ()
  "setup initials buffers and windows as I like them"
  (interactive)
  (eshell)
  (split-window-right)
  (other-window 1)
  (find-file gg-notes-file)
  (find-file gg-todo-file)
  (find-file gg-done-file))


(defun gg/yt-playlist-to-org (playlist-url)
  "turn a youtube playlist link into an org section, where each video is a subsection"
  (interactive "sPlaylist url or id: ")
  (let*
      ((header-cmd (concat "youtube-dl"
			   " --ignore-errors --get-filename "
			   " --output '* [/] [[https://www.youtube.com/playlist?list=%(playlist_id)s][%(uploader)s - %(playlist_title)s]]' "
			   " --playlist-end 1 "))
       (playlist-header (shell-command-to-string (concat header-cmd playlist-url)))
       (entries-cmd (concat "youtube-dl"
			    " --ignore-errors --get-filename "
			    " --output '- [ ] [[https://www.youtube.com/watch?v=%(id)s][%(title)s]]' "))
       (playlist-entries (shell-command-to-string (concat entries-cmd playlist-url))))
    (insert (concat "\n" playlist-header playlist-entries "\n"))))


(defun gg/get-string-from-file (filePath)
  "Return file content as string."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))


(defun gg/get-computer-model ()
  (interactive)
  (let ((dmi-product-family-file "/sys/devices/virtual/dmi/id/product_family"))
    (if (file-readable-p dmi-product-family-file)
	(string-trim (gg/get-string-from-file dmi-product-family-file))
      "unknown")))


(defun gg/is-computer-model? (model)
  (interactive)
  (string= model (gg/get-computer-model)))


(defun gg/movie-pirate (url name)
  "Asynchronously download URL to ~/dl/NAME.mkv using ffmpeg.
NAME is transformed to lowercase and spaces are replaced with underscores."
  (interactive
   (list
    (read-string "URL: ")
    (read-string "Name: ")))
  (let* ((safe-name (replace-regexp-in-string
                     " +" "_"
                     (downcase name)))
         (output (expand-file-name
                  (concat safe-name ".mkv")
                  (expand-file-name "dl" (getenv "HOME"))))
         (process-name (format "ffmpeg-%s" safe-name)))
    (start-process
     process-name
     (format "*%s*" process-name)
     "ffmpeg"
     "-i" url
     "-c" "copy"
     output)))

;;; UX

;; type y instead of yes
(defalias 'yes-or-no-p 'y-or-n-p)

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


;;; Visuals

;; set default font and size
(set-face-attribute
 'default nil :family "Source Code Pro" :height 130)

;; do not show splash screen
(setq inhibit-startup-screen 1)

;; remove toolbar
(tool-bar-mode -1)

;; remove menu bar
(menu-bar-mode -1)

;; scrollbar on the right
(set-scroll-bar-mode 'right)

;; visible bell
(setq visible-bell t)

;; show line number on the side
(global-display-line-numbers-mode)

;; column number in info bar
(column-number-mode 1)

;; *scratch* default text

(setq initial-scratch-message "\
;;                    | |     | |    \n\
;;  ___  ___ _ __ __ _| |_ ___| |__  \n\
;; / __|/ __| '__/ _` | __/ __| '_ \\ \n\
;; \\__ \\ (__| | | (_| | || (__| | | |\n\
;; |___/\\___|_|  \\__,_|\\__\\___|_| |_|\n\n")


;;; Keybindings

;; keybinding to most used file
(global-set-key (kbd "<f5>")
		'gg/apparecchia)

;; missing ctrl-A from vim
(global-set-key (kbd "C-c +")
		'gg/increment-number-at-point)

;; kill buffer without confirmation
(global-set-key (kbd "C-x k")
		'kill-current-buffer)

;; toggle line truncation on/off
(global-set-key (kbd "C-c w")
		'visual-line-mode)

;; on my new Lenovo E14 gen5
;; Fn key is interpreded as <WakeUp>
;; so we ignore it
(when (gg/is-computer-model? "ThinkPad E14 Gen 5")
  (global-set-key (kbd "<WakeUp>") 'ignore))
