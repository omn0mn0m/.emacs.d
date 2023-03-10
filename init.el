;; ---------------------------------------------------------------------------
;; Package Stuff
;; ---------------------------------------------------------------------------
(package-initialize)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                          ("elpa" . "http://tromey.com/elpa/")
                          ("melpa" . "http://melpa.org/packages/")))

;; use-package setup
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))
(use-package auto-compile
  :config (auto-compile-on-load-mode))
(setq load-prefer-newer t)
  
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

;; Sentences end with a single space
(setq sentence-end-double-space nil)

;; Replaces marked region with what is being typed
(delete-selection-mode 1)

;; Turn off tabs
(setq-default indent-tabs-mode nil)

;; Projectile
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))
  
;; Minibuffer Editing
(use-package miniedit
  :commands minibuffer-edit
  :init (miniedit-install))

;; Fido Mode
(fido-mode)

;; Indenting
(use-package aggressive-indent)
(global-aggressive-indent-mode 1)

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

;; Web Dev
(use-package elixir-mode)

(use-package web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(use-package pug-mode)

;; PlatformIO
(use-package platformio-mode)

;; Markdown
(use-package markdown-mode)

;; LSP Stuff
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred


;; ---------------------------------------------------------------------------
;; Other Packages
;; ---------------------------------------------------------------------------
(use-package org)
(electric-indent-mode -1)

(use-package ledger-mode)

;; Elcord - For Discord Rich Presence
(use-package elcord)
(elcord-mode)

(use-package company)
(add-hook 'after-init-hook 'global-company-mode)
