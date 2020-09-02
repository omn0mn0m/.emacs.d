;; ---------------------------------------------------------------------------
;; Package Stuff
;; ---------------------------------------------------------------------------
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("marmalade" . "http://marmalade-repo.org/packages/")
                          ("elpa" . "http://tromey.com/elpa/")
                          ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; use-package setup
(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)

(require 'bind-key)

;; Load git submodules location
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; ---------------------------------------------------------------------------
;; Cosmetic changes
;; ---------------------------------------------------------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/dracula")
(load-theme 'dracula t)

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Telephone Line
(use-package telephone-line)
(telephone-line-mode 1)

;; Startup Screen
(defun my-inhibit-startup-screen-always ()
  "Startup screen inhibitor for `command-line-functions`.
Inhibits startup screen on the first unrecognised option."
  (ignore (setq inhibit-startup-screen t)))

(add-hook 'command-line-functions #'my-inhibit-startup-screen-always)

;; ---------------------------------------------------------------------------
;; Behavioural Changes
;; ---------------------------------------------------------------------------
;; Replaces yes or no with y or n shorthands
(fset 'yes-or-no-p 'y-or-n-p)

;; Replaces marked region with what is being typed
(delete-selection-mode 1)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

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
(use-package magit
 :bind (("C-x g" . magit)))

;; LaTeX
(use-package tex
  :ensure auctex)
(use-package latex-preview-pane)
(add-hook `LaTeX-mode-hook `latex-preview-pane-mode)

;; Ren'Py
(load "renpy")

;; ---------------------------------------------------------------------------
;; Other Packages
;; ---------------------------------------------------------------------------
(use-package org)

;; Elcord - For Discord Rich Presence
(load "elcord")
(elcord-mode)
