;; ---------------------------------------------------------------------------
;; Package Stuff
;; ---------------------------------------------------------------------------
(package-initialize)

(require 'package)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa") t))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

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

;; Dired
(use-package dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

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

;; ---------------------------------------------------------------------------
;; Default init.el config
;; ---------------------------------------------------------------------------
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (dired-details miniedit smart-mode-line-powerline-theme smart-mode-line auto-compile use-package magit)))
 '(python-shell-completion-native-enable nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
