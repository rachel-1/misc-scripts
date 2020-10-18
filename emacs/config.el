;; setup package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; manually install use-package package manager
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; always download and install use-package packages
(setq use-package-always-ensure t)
;; prefer latest, not stable
(setq use-package-always-pin "melpa")
(eval-when-compile
  (require 'use-package))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (tango-dark)))
 '(inhibit-startup-screen t)
 '(ivy-mode t)
 '(package-selected-packages
   (quote
    (clang-format flycheck use-package swiper ivy magit)))
 '(safe-local-variable-values
   (quote
    ((flycheck-clang-warnings "all" "extra" "no-pragma-once-outside-header")
     (flycheck-clang-language-standard . "c++14")
     (flycheck-gcc-language-standard . "c++14")
     (flycheck-c/c++-clang-executable . "/usr/bin/clang")
     (c-file-offsets
      (access-label . -3)
      (innamespace . 0))
     (c-default-style "google")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package magit)
(global-set-key (kbd "C-x g") 'magit-status)

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
(defadvice other-frame (before other-frame-now activate)
  (prelude-auto-save-command))

(setq-default fill-column 80)
(require 'comint)
(setq comint-password-prompt-regexp
      (concat comint-password-prompt-regexp
              "\\|^Password for '.*': \\s *\\'"))

(define-skeleton skel-python-print
  "Insert a python print statement"
  "Type name of variable: "
  "print(\"" str ": \", "str") # TODO(rachel0) - remove debug statement"_)

(global-set-key (kbd "C-x C-p") 'skel-python-print)

(define-skeleton skel-c++-print
  "Insert a C++ print statement"
  "Type name of variable: "
  "cout << \"" str ": \" << "str" << endl; // TODO(rachel0) - remove debug statement"_)

;;(global-set-key (kbd "C-x C-p") 'skel-python-print)
(require 'cc-mode)
(define-key c++-mode-map (kbd "C-x C-p") 'skel-c++-print)

;;(require 'python-mode)
;;(define-key python-mode-map (kbd "C-x C-p") 'skel-python-print)

;;(add-to-list 'load-path "~/.emacs.d/lisp/")
;;(require 'xcscope)
;;(cscope-setup)

(setq-default indent-tabs-mode nil)
(global-auto-revert-mode t)

;; Bazel files are basically Python
(add-to-list 'auto-mode-alist '("\\BUILD\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl\\'" . python-mode))

(defun bazel-build-command (filename)  
  "Get a bazel build command for the file."
  (let ((ext (downcase (file-name-extension filename t)))
	(root (file-name-base filename)))
    (or (if (string-suffix-p "test.cc" filename) (concat "bazel test --test_output=all :" root))
	(if (string-suffix-p "test.py" filename) (concat "bazel test --test_output=all :" root))
	(if (string-suffix-p "_benchmark.cc" filename) (concat "bazel run :" root))
	(if (string= ext ".stack") (concat "bazel build :" root "_stack"))
	(if (string= ext ".proto") (concat "bazel build :" root "_proto"))
	(if (string= ext ".cc") (concat "bazel build :" root))
	(if (string= ext ".hh") (concat "bazel build :" root))
	(if (string= ext ".py") (concat "bazel build :" root))
	(if (string= ext ".sh") (concat "bazel build :" root)))
    )
  )
(defun do-bazel-build ()
  "Run the bazel build for the current buffer."
  (interactive)
  (save-some-buffers 1)
  (compile (bazel-build-command (buffer-file-name)))
  )
(global-set-key (kbd "C-<f5>") 'do-bazel-build)

(defun do-recompile ()
  "Recompile."
  (interactive)
  (save-some-buffers 1)
  (switch-to-buffer-other-window "*compilation*")
  (recompile)
  (other-window 1)
  )

(global-set-key (kbd "<f5>") 'do-recompile)

(defun re-run-in-shell ()
  "Re-run last command in shell."
  (interactive)
  (save-some-buffers 1)
  (if (get-buffer-window "*shell*" 'visible)
       (with-current-buffer (get-buffer-create "*shell*")
       (insert (comint-previous-input-string 0))
       (comint-send-input))
       (progn (pop-to-buffer "*shell*")
       (insert (comint-previous-input-string 0))
       (comint-send-input))
  )
)

(defun re-run-in-shell-2 ()
  "Re-run last command in shell."
  (interactive)
  (save-some-buffers 1)
  (if (get-buffer-window "*shell*<2>" 'visible)
       (with-current-buffer (get-buffer-create "*shell*<2>")
       (insert (comint-previous-input-string 0))
       (comint-send-input))
       (progn (pop-to-buffer "*shell*<2>")
       (insert (comint-previous-input-string 0))
       (comint-send-input))
  )
)

(global-set-key (kbd "<f6>") 're-run-in-shell)
(global-set-key (kbd "<f7>") 're-run-in-shell-2)

(defun lint ()
  "Call linter."
  (interactive)
  (shell-command "arc lint --apply-patches")
  )

(global-set-key (kbd "C-;") 'lint)
