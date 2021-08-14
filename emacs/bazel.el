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
