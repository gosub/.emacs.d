;;; -*- lexical-binding: t; -*-

;; Disable UI elements early to prevent flash of default Emacs

(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
