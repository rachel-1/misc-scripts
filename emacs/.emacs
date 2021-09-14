;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

; Install any necessary packages.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(load "~/.emacs.d/config/ssh.el")
(if (load "~/.emacs.d/config/org.el")
    (load "~/.emacs.d/config/org-roam.el"))
(load "~/.emacs.d/config/references.el")
(load "~/.emacs.d/config/autocomplete.el")
(load "~/.emacs.d/config/autosave.el")
(load "~/.emacs.d/config/spell-checking.el")
(load "~/.emacs.d/config/code.el")
(load "~/.emacs.d/config/bazel.el")
(load "~/.emacs.d/config/ui.el")

(global-set-key (kbd "C-c C-u") #'browse-url-at-point)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(poet))
 '(custom-safe-themes
   '("2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" default))
 '(deft-use-filename-as-title t)
 '(package-selected-packages
   '(ox-clip org-attach-screenshot ox-slack magit org-roam helm-ispell helm-flyspell org-pomodoro deft use-package poet-theme org-roam-bibtex org-noter-pdftools org-bullets olivetti ewal-spacemacs-themes emacsql-sqlite counsel))
 '(safe-local-variable-values
   '((flycheck-clang-warnings "all" "extra" "no-pragma-once-outside-header")
     (flycheck-clang-language-standard . "c++17")
     (flycheck-gcc-language-standard . "c++17")
     (flycheck-c/c++-clang-executable . "/usr/bin/clang")
     (c-file-offsets
      (access-label . -3)
      (innamespace . 0))
     (c-default-style "google"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit org-code :extend t :background "#e0e0e0" :height 0.75))))
 '(org-code ((t (:inherit nil :family "Consolas"))))
 '(org-table ((t (:inherit fixed-pitch :background "#e0e0e0" :height 0.8 :width normal :family "Courier New")))))
