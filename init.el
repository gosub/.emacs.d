;; additional package repositories

(add-to-list 'package-archives
  '("melpa" . "https://melpa.org/packages/"))

;; packages

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

(use-package org
  :defer t
  :init
  (setq org-clock-sound
	"~/dl/audio/alarm.wav")
  (setq org-todo-keywords
	'((sequence "TODO" "ASIDE" "DONE")))
  (setq org-todo-keyword-faces
	'(("ASIDE" . "dark blue")))
  (setq org-startup-folded t)
  :hook
  (org-mode . (lambda ()
		"disable line numbers in org-mode, too distracting"
		(display-line-numbers-mode 0)
		(electric-indent-mode -1))))

;; personal customizations (cfg folder, gg- prefix)

(add-to-list 'load-path
  (expand-file-name "cfg" user-emacs-directory))


;; personal configurations

(require 'gg-dirs-and-files)
(require 'gg-functions)
(require 'gg-ux)
(require 'gg-visuals)
(require 'gg-unicode)
(require 'gg-erlang)
(require 'gg-keybindings)
(require 'gg-tidal)
(require 'gg-cpp)
