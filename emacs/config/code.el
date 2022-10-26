
(define-skeleton skel-python-print
  "Insert a python print statement"
  "Type name of variable: "
  "print(\"" str ": \", "str") # TODO(rachel0) - remove debug statement"_)

(global-set-key (kbd "C-x C-p") 'skel-python-print)

(defun add-breakpoint ()
  (interactive)
  (indent-for-tab-command)
  (insert "import pdb; pdb.set_trace()")
  (boon-newline-dwim)
)

(define-skeleton skel-c++-print
  "Insert a C++ print statement"
  "Type name of variable: "
  "std::cout << \"" str ": \" << "str" << std::endl; // TODO(rachel-1) - remove debug statement"_)

(require 'cc-mode)
(define-key c++-mode-map (kbd "C-x C-p") 'skel-c++-print)


;; TODO: python mode doesn't work?
;;(require 'python-mode)
;;(define-key python-mode-map (kbd "C-x C-p") 'skel-python-print)

;;(add-to-list 'load-path "~/.emacs.d/lisp/")
;;(require 'xcscope)
;;(cscope-setup)


;; Use spaces instead of tabs and list the column number
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default fill-column 100)
(setq indent-line-function 'insert-tab)
(setq column-number-mode t)

;; Make sure Emacs buffers change as I switch around on git
(global-auto-revert-mode t)

(defun lint ()
  "Call linter."
  (interactive)
  (shell-command "arc lint --apply-patches")
  )

(global-set-key (kbd "C-c ;") 'lint)

; Use Magit to have a UI for Git
(use-package magit)

(setq pop-up-windows nil)
(defun magit-status-av-repo ()
  "Magit status, but with repo hard-coded so it works from anywhere."
  (interactive)
  (magit-status "~/av")
)
(global-set-key (kbd "C-x g") 'magit-status-av-repo)

;  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
;  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
;  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
;  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
;  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
;  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

(defun magit-status-av-worktree-repo ()
  "Magit status, but with repo hard-coded so it works from anywhere."
  (interactive)
  (magit-status "~/worktree")
)
(global-set-key (kbd "C-x w") 'magit-status-av-worktree-repo)

(defun is-worktree ()
  (unless (not
(string-match-p (regexp-quote "av|worktree") (buffer-file-name)))
(error "Invalid path"))
 (string-match-p (regexp-quote "worktree") (buffer-file-name)))
  
(defun toggle-between-repo-and-worktree ()
  "Switch a buffer between ~/av/file and ~/worktree/file."
  (interactive)
    (if (is-worktree)
        (find-file (replace-regexp-in-string "worktree" "av" (buffer-file-name)))
        (find-file (replace-regexp-in-string "av" "worktree" (buffer-file-name)))
    )
  )
(global-set-key (kbd "C-x C-o") 'toggle-between-repo-and-worktree)

; Define a hydra for smerge because it has stupid default keybindings.
(use-package hydra) 
(defhydra hydra-smerge (:color red :hint nil
:pre (smerge-mode 1))
"
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
_n_ext _b_ase _R_efine _<_: base-mine
_p_rev _m_ine _E_diff _=_: mine-other
^ ^ _o_ther _C_ombine _>_: base-other
^ ^ _a_ll _r_esolve
_q_uit _RET_: current
"
("RET" smerge-keep-current)
("C" smerge-combine-with-next)
("E" smerge-ediff)
("R" smerge-refine)
("a" smerge-keep-all)
("b" smerge-keep-base)
("m" smerge-keep-mine)
("n" smerge-next)
("o" smerge-keep-other)
("p" smerge-prev)
("r" smerge-resolve)
("<" smerge-diff-base-mine)
("=" smerge-diff-mine-other)
(">" smerge-diff-base-other)
("q" nil :color blue))

(global-set-key (kbd "C-c C-m") 'hydra-smerge/body)
        
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

; Make sure TRAMP always uses bash
(setq explicit-shell-file-name "/bin/bash")

;; Ignore duplicates when searching through history.
(setq comint-input-ignoredups t)

; Create a new shell, incrementing the number each time.
(defun new-shell (&optional directory)
  (interactive)

  (let (
        (currentbuf (get-buffer-window (current-buffer)))
        (newbuf     (generate-new-buffer-name "*shell*"))
	)

    (let ((default-directory directory))
    (generate-new-buffer newbuf)
    )
    (set-window-dedicated-p currentbuf nil)
    (set-window-buffer currentbuf newbuf)
    (shell newbuf)
    (if directory
    (with-current-buffer newbuf
    (shell-cd directory))
    ()) 
    )
  )

(global-set-key (kbd "C-t") nil) ;; Remove twiddle
(global-set-key (kbd "C-t") 'new-shell)
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(kill-matching-buffers "\*shell\*.*" nil t)
(new-shell "~/av")
(new-shell "~/worktree")
(new-shell "~/av")
(new-shell "~/worktree")
(defun get-shell ()
    (interactive)
    (if (is-worktree)
        (switch-to-buffer-other-window "*shell*<2>")
        (switch-to-buffer-other-window "*shell*")
        )
        )
(defun get-shell-two ()
    (interactive)
    (if (is-worktree)
        (switch-to-buffer-other-window "*shell*<4>")
        (switch-to-buffer-other-window "*shell*<3>")
        )
        )

(defun re-run-in-shell ()
  "Re-run last command in shell."
  (interactive)
  (save-some-buffers 1)
  (if (get-buffer-window "*shell*" 'visible)
       (with-current-buffer (get-buffer-create "*shell*")
       (end-of-buffer)
       (insert (comint-previous-input-string 0))
       (comint-send-input))
       (progn (pop-to-buffer "*shell*")
       (insert (comint-previous-input-string 0))
       (comint-send-input))
       (recenter-top-bottom)
  )
)

(defun re-run-in-shell-2 ()
  "Re-run last command in shell."
  (interactive)
  (save-some-buffers 1)
  (if (get-buffer-window "*shell*<2>" 'visible)
       (with-current-buffer (get-buffer-create "*shell*<2>")
       (end-of-buffer)
       (insert (comint-previous-input-string 0))
       (comint-send-input))
       (progn (pop-to-buffer "*shell*<2>")
       (insert (comint-previous-input-string 0))
       (comint-send-input))
  )
)

(defun magit-status-av-repo ()
  "Magit status, but with repo hard-coded so it works from anywhere."
  (interactive)
  (magit-status "~/av")
)

;(defun magit-smart-status ()
;    (if (string-match-p (regexp-quote "worktree") (buffer-file-name))

(defun magit-rebase-on-master ()
  (interactive)
  (magit-fetch-refspec "origin" "master:master" "")
  (magit-rebase-onto-upstream nil)
)

(defun magit-stage-and-commit ()
  (interactive)
  (magit-stage-modified t)
  (magit-commit-create)
  )

(global-set-key (kbd "<f6>") (lambda() (interactive) (switch-to-buffer-other-window "*shell*")))
(global-set-key (kbd "C-<f6>") 're-run-in-shell)
(global-set-key (kbd "<f7>") (lambda() (interactive) (switch-to-buffer-other-window "*shell*<2>")))
(global-set-key (kbd "C-<f7>") 're-run-in-shell-2)

(require 'boon-qwerty)
(add-hook 'prog-mode-hook 'turn-on-boon-mode)
(global-set-key (kbd "C-v") 'boon-set-command-state)
(define-key boon-command-map "H" 'projectile-find-other-file)
(define-key boon-command-map "F" 'counsel-yank-pop)
(define-key boon-command-map "c" 'recenter-top-bottom)
(define-key boon-command-map "" 'boon-newline-dwim)
(define-key boon-command-map "r" 'swiper)
(define-key boon-command-map "e" 'add-breakpoint)

; Make sure programs executed from the shell use the current Emacs as editor.
; This is most important for Git, which is why `with-editor ships with Magit.
(add-hook 'shell-mode-hook  'with-editor-export-editor)

; Use projectile for projects.    
(use-package helm-projectile
    :config
    (helm-projectile-on)
    (projectile-mode +1)
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    ;; Add "bazel" project type
    (projectile-register-project-type
    'bazel
    '("WORKSPACE")  ;; identifier file for a bazel project type
    :compile "bazel build "
    :test "bazel test "
    :run "bazel run "
    :test-prefix "test_"
    :test-suffix "_test")
    :delight
    )
; Turn off mode line
;(setq projectile-mode-line-prefix "")
    ;(eval-after-load "helm-projectile"
;'(setq projectile-mode-line "TEST")
;)

(add-hook 'compilation-mode-hook (lambda () (local-set-key (kbd "n") 'compilation-next-error)
                                            (local-set-key (kbd "p") 'compilation-previous-error)
                                            (local-set-key (kbd "o") 'compilation-next-error)
                                            (local-set-key (kbd "i") 'compilation-previous-error)
                                            (local-set-key (kbd "x o") 'other-window)
                                            (local-set-key (kbd "x 2") 'split-window-below)
                                            (local-set-key (kbd "x b") 'ivy-switch-buffer)
                                            (local-set-key (kbd "l") 'recenter-top-bottom)
                                 )
)

; Scroll with cursor in the center of the screen
;(setq maximum-scroll-margin 0.5
;      scroll-margin 99999
;      scroll-preserve-screen-position t
;      scroll-conservatively 0)
    
; Support for jump-to-definition
; By default uses M-. to jump to definition.
(use-package dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
; Use a helm wrapper for xref.
(use-package helm-xref)        


(use-package multiple-cursors)
(global-set-key (kbd "M-RET") 'mc/edit-lines)

(global-set-key (kbd "M-o") 'ace-window)
(setq aw-scope 'frame)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

; MAGIC from https://github.com/magit/magit/issues/2541
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer
         (cond ((and (derived-mode-p 'magit-mode)
                     (eq (with-current-buffer buffer major-mode)
                         'magit-status-mode))
                nil)
               ((memq (with-current-buffer buffer major-mode)
                      '(magit-process-mode
                        magit-revision-mode
                        magit-diff-mode
                        magit-stash-mode))
                nil)
               (t
                '(display-buffer-same-window))))))
