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
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

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

(add-hook 'text-mode-hook
           (lambda ()
            (variable-pitch-mode 1)))

(set-face-attribute 'default nil :family "DejaVu Sans Mono" :height 200)
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

(use-package poet-theme)

;; Clean up the mode line
(require 'delight)
(delight '(
           (projectile-mode nil "helm-projectile")
           (ivy-mode nil "ivy")
           (yas-minor-mode nil "yasnippet")
           (clipetty-mode nil "clipetty")
           (eldoc-mode nil "eldoc")
           (flyspell-mode nil "flyspell")
           (abbrev-mode nil "abbrev")
           )
         )

(defun switch-mode-line-color ()
  (message "switching")
  (if (string-match-p (regexp-quote "av") (buffer-file-name))
      (face-remap-add-relative
       'mode-line '((:foreground "ivory" :background "DarkOrange2") mode-line)
       )
    (if (string-match-p (regexp-quote "worktree") (buffer-file-name))
        (face-remap-add-relative
         'mode-line '((:foreground "ivory" :background "DarkBlue2") mode-line)
         )
      )
    )
  )

;(add-hook 'find-file-hook 'switch-mode-line-color)

(advice-add 'vc-git-mode-line-string
            :override (lambda (file) ""))

(setq org-clock-mode-line-total 'current)
(setq org-clock-clocked-in-display 'nil)
(defun minibuffer-line--update ()
(with-current-buffer " *Minibuf-0*"
  (erase-buffer)
  (insert (format "%s %s" git-line clock-line))
  )
)
(defun update-clock-line ()
  (let* ((clocked-mins (mod (org-time-convert-to-integer
		                                (org-time-since org-clock-start-time))
		                               60)
                                )
        (clocked-hours (floor clocked-mins 60))
        )
    (setq clock-line (format "[%d:%d] %s" clocked-hours clocked-mins org-clock-current-task))
    )
  (minibuffer-line--update)
  )

(defun format-branch (branch-name)
  "Remove rachel-1 from the start of the branch name, if present."
  (if (and branch-name
           (string-match-p "^rachel-1/" branch-name))
      (substring branch-name 9)
    branch-name
    )
  )

(defun update-git-line ()
  (let ((default-directory "~/av")
         )
    (setq main-branch (format-branch (magit-get-current-branch)))
    )
  (let ((default-directory "~/worktree")
        )
    (setq worktree-branch (format-branch (magit-get-current-branch)))
    )
  (setq git-line (format "%s | %s" main-branch worktree-branch))
  (minibuffer-line--update)
)

(setq clock-line "")
(update-git-line)
(add-hook 'org-clock-in-hook 'update-clock-line)
(add-hook 'org-clock-out-hook 'update-clock-line)
; Run once a minute.
(run-with-timer 0 60 'update-clock-line)
(add-hook 'magit-refresh-buffer-hook 'update-git-line)
