;; Ivy, Counsel, & Swiper
;; Enable Ivy mode in general
(use-package ivy)
(use-package counsel)
(use-package swiper)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

;; Add Counsel and Swiper search functions
(global-set-key (kbd "C-c f r") #'counsel-recentf)

;; Replace defaults with Counsel+Swiper versions
(global-set-key (kbd "C-s") #'swiper)
(global-set-key (kbd "M-x") #'counsel-M-x)
(global-set-key (kbd "C-x C-f") #'counsel-find-file)
(global-set-key (kbd "M-y") #'counsel-yank-pop)

;; Optionally, you can replace these default functions with Counsel version, too
;;(global-set-key (kbd "C-h f") 'counsel-describe-function)
;;(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-x C-f") #'counsel-find-file)

(use-package yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/config/snippets"))
(yas/load-directory "~/.emacs.d/config/snippets")
(setq yas/indent-line nil)
(yas-global-mode 1)
