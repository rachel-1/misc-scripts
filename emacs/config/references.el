(use-package org-ref)
(use-package org-roam-bibtex)
(use-package helm)
(use-package helm-bibtex)

;; Org-ref
;; Set up bibliography
(setq org-ref-default-bibliography '("G:/My Drive/zotero.bib"))
(setq bibtex-completion-bibliography "G:/My Drive/zotero.bib")
;(setq orb-insert-interface 'helm-bibtex)
(global-set-key (kbd "<f8>") 'org-ref-helm-insert-cite-link)

;; Org-roam-bibtex
(require `org-roam-bibtex)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)
(define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions)

; PDF-tools
(use-package pdf-tools)
(pdf-tools-install)
(use-package org-noter)

; Setup with ORB (other setup done in org-roam)
(setq orb-preformat-keywords
      '("citekey" "title" "url" "author-or-editor" "keywords" "file")
      orb-process-file-keyword t
      orb-file-field-extensions '("pdf"))

;(add-to-list 'org-roam-capture-templates
;      '(("r" "bibliography reference" plain
;         (file "citation_template")
;         :if-new
;         (file+head "references/${citekey}.org" "#+title: ${title}\n"))))

;;; Open PDF file stored in Windows' standard Zotero storage
(setq bibtex-completion-pdf-field "File")

; Change what the link description will be in various text modes
;(setq bibtex-completion-format-citation-functions
;  '((org-mode      . bibtex-completion-format-citation-org-title-link-to-PDF)
;    (latex-mode    . bibtex-completion-format-citation-cite)
;    (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
;    (default       . bibtex-completion-format-citation-default)))
