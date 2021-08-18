
(define-skeleton skel-python-print
  "Insert a python print statement"
  "Type name of variable: "
  "print(\"" str ": \", "str") # TODO(rachel0) - remove debug statement"_)

(global-set-key (kbd "C-x C-p") 'skel-python-print)

(define-skeleton skel-c++-print
  "Insert a C++ print statement"
  "Type name of variable: "
  "cout << \"" str ": \" << "str" << endl; // TODO(rachel0) - remove debug statement"_)

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
(setq column-number-mode t)

;; Make sure Emacs buffers change as I switch around on git
(global-auto-revert-mode t)

(defun lint ()
  "Call linter."
  (interactive)
  (shell-command "arc lint --apply-patches")
  )

(global-set-key (kbd "C-;") 'lint)

; Use Magit to have a UI for Git
(use-package magit)

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

; Create a new shell, incrementing the number each time.
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
