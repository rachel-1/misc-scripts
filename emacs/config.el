(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tango-dark)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defun new-shell ()
  (interactive)

  (let (
        (currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name "*shell*"))
	)

    (generate-new-buffer newbuf)
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)
    )
  )

(global-set-key (kbd "C-t") nil) ;; Remove twiddle
(global-set-key (kbd "C-t") 'new-shell)

;; Taken from https://www.emacswiki.org/emacs/IndentRigidlyN
;; (originally by KragenJavierSitaker)
(defun indent-rigidly-n (n)
  "Indent the region, or otherwise the current line, by N spaces."
  (let* ((use-region (and transient-mark-mode mark-active))
         (rstart (if use-region (region-beginning) (point-at-bol)))
         (rend   (if use-region (region-end)       (point-at-eol)))
         (deactivate-mark "irrelevant")) ; avoid deactivating mark
    (indent-rigidly rstart rend n)))
(defun indent-rigidly-4 ()
  "Indent the region, or otherwise the current line, by 4 spaces."
  (interactive)
  (indent-rigidly-n 4))
(defun outdent-rigidly-4 ()
  "Indent the region, or otherwise the current line, by -4 spaces."
  (interactive)
  (indent-rigidly-n -4))

(global-set-key (kbd "C-x <") nil)
(global-set-key (kbd "C-x >") nil)
(global-set-key (kbd "C-x <") 'outdent-rigidly-4)
(global-set-key (kbd "C-x >") 'indent-rigidly-4)

(global-set-key (kbd "M-n")
                (lambda () (interactive) (forward-line  5)))
(global-set-key (kbd "M-p")
		(lambda () (interactive) (forward-line -5)))

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

(setq-default fill-column 80)
(require 'comint)
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|^Password for '.*': \\s *\\'"))

(define-skeleton skel-python-print
  "Insert a python print statement"
  "Type name of variable: "
  "print(\"" str ": \", "str") # TODO - remove debug statement"_)


(define-skeleton skel-c++-print
  "Insert a C++ print statement"
  "Type name of variable: "
  "cout << \"" str ": \" << "str" << endl; // TODO - remove debug statement"_)

;;(global-set-key (kbd "C-x C-p") 'skel-python-print)
(require 'cc-mode)
(define-key c++-mode-map (kbd "C-x C-p") 'skel-c++-print)

;;(require 'python-mode)
;;(define-key python-mode-map (kbd "C-x C-p") 'skel-python-print)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'xcscope)
(cscope-setup)

(setq-default indent-tabs-mode nil)
(global-auto-revert-mode t)
