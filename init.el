;; ---------------------------------------------------------------------------
;; Package Stuff
;; ---------------------------------------------------------------------------
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'package)
(package-initialize)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")
                          ("elpa" . "http://tromey.com/elpa/")
                          ("melpa" . "http://melpa.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))

(require 'bind-key)

;; ---------------------------------------------------------------------------
;; Cosmetic changes
;; ---------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/dracula")
(load-theme 'dracula t)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Mode Line Changes
(use-package smart-mode-line)
(use-package smart-mode-line-powerline-theme)
(setq sml/theme 'powerline)
(setq sml/no-confirm-load-theme t)  ;; Workaround to always asking for load
(sml/setup)

;; Startup Screen
(defun my-inhibit-startup-screen-always ()
  "Startup screen inhibitor for `command-line-functions`.
Inhibits startup screen on the first unrecognised option."
  (ignore (setq inhibit-startup-screen t)))

(add-hook 'command-line-functions #'my-inhibit-startup-screen-always)

;; Neotree for tree file explorer
(use-package treemacs
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; ---------------------------------------------------------------------------
;; Behavioural Changes
;; ---------------------------------------------------------------------------
(fset 'yes-or-no-p 'y-or-n-p)

;; ---------------------------------------------------------------------------
;; Programming Stuff
;; ---------------------------------------------------------------------------

;; Octave
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; Arduino
(setq auto-mode-alist
      (cons '("\\.ino$" . c-mode) auto-mode-alist))

;; Magit
;;(use-package magit)

;; LaTeX
(use-package tex
  :ensure auctex)
(use-package latex-preview-pane)
(add-hook `LaTeX-mode-hook `latex-preview-pane-mode)

;; Ren'Py
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "renpy")

;; ---------------------------------------------------------------------------
;; Other Packages
;; ---------------------------------------------------------------------------
(use-package org)

;; ---------------------------------------------------------------------------
;; Default init.el config
;; ---------------------------------------------------------------------------
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
