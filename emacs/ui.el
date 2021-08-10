;; Optional additional aesthetic changes
;; Adapted from `sanity.el' in Elegant Emacs by Nicolas P. Rougier (rougier)
;; https://github.com/rougier/elegant-emacs

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq pop-up-windows nil)
(tool-bar-mode 0) 
(tooltip-mode  0)
(scroll-bar-mode 0)
(menu-bar-mode 0) ;menu bar is explicitly turned on for beginners. Change the value to 0.

;; Optional aditional aesthetic changes
;; Adapted from `elegance.el' in Elegant Emacs by Nicolas P. Rougier (rougier)
;; https://github.com/rougier/elegant-emacs

;; Line cursor and no blink
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode t)

;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

; The org-bullets package replaces all headline markers with different Unicode bullets:
(use-package org-bullets
     :config
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
(setq org-bullets-bullet-list '("◉" "○" "•" "-"))

(add-hook 'text-mode-hook
           (lambda ()
            (variable-pitch-mode 1)))

(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 130)
(set-face-attribute 'fixed-pitch nil :family "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil :family "Constantia")

; modify all the org levels to be the same size
(setq poet-theme-variable-headers nil)

; make variable pitch larger (from its default of 1.23)
(setq poet-theme-variable-pitch-multiplier 1.5)

; handle wrapping properly
(setq org-startup-indented t)
;(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Olivetti
;; Look & Feel for long-form writing
(use-package olivetti)

;; Set the body text width
(setq-default olivetti-body-width 100)

;; Enable Olivetti for text-related mode such as Org Mode
(add-hook 'text-mode-hook 'olivetti-mode)
