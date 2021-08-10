;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

; Install any necessary packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(load "~/.emacs.d/org.el")
(load "~/.emacs.d/org-roam.el")
(load "~/.emacs.d/references.el")
(load "~/.emacs.d/autocomplete.el")
(load "~/.emacs.d/ui.el")

;; Spell checking
(setenv "LANG" "en_US")
(setq ispell-program-name
      "C:/Users/rache/bin/hunspell/bin/hunspell.exe")
(global-set-key (kbd "C-x c") #'helm-flyspell-correct)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Auto-save
; Taken from https://github.com/bbatsov/prelude/commit/f66a56a72edd38518a96cbba88822957275d108c
(defcustom prelude-auto-save t
  "Non-nil values enable Prelude's auto save."
  :type 'boolean
  :group 'prelude)

(defun prelude-auto-save-command ()
  (when (and prelude-auto-save
             buffer-file-name
             (buffer-modified-p (current-buffer)))
    (save-buffer)))

(defadvice switch-to-buffer (before save-buffer-now activate)
  (prelude-auto-save-command))
(defadvice other-window (before other-window-now activate)
  (prelude-auto-save-command))
(defadvice other-frame (before other-frame-now activate)
  (prelude-auto-save-command))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(poet))
 '(custom-safe-themes
   '("2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" default))
 '(org-agenda-files
   '("c:/Users/rache/Google Drive/Org/process.org" "c:/Users/rache/Google Drive/Org/20210803140910-engineering_notebook.org"))
 '(org-pomodoro-format "Work~%s")
 '(org-pomodoro-keep-killed-pomodoro-time t)
 '(org-pomodoro-manual-break t)
 '(package-selected-packages
   '(helm-ispell helm-flyspell org-pomodoro deft use-package poet-theme org-roam-bibtex org-noter-pdftools org-bullets olivetti ewal-spacemacs-themes emacsql-sqlite counsel)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil)))
 '(org-block ((t (:inherit org-code :extend t :background "#e0e0e0"))))
 '(org-code ((t (:inherit nil :family "Consolas"))))
 '(org-table ((t (:inherit fixed-pitch :background "#e0e0e0" :height 0.8 :width normal :family "Courier New")))))
