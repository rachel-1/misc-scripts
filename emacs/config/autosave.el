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

(defadvice ace-window (before save-buffer-now activate)
  (prelude-auto-save-command))
(defadvice switch-to-buffer (before save-buffer-now activate)
  (prelude-auto-save-command))
(defadvice other-window (before other-window-now activate)
  (prelude-auto-save-command))
(defadvice other-frame (before other-frame-now activate)
  (prelude-auto-save-command))

;(desktop-load-default)
(desktop-read)
