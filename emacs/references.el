(use-package org-ref)
(use-package org-roam-bibtex)
(use-package helm)
(use-package helm-bibtex)

;; Org-ref
;; Set up bibliography
(setq org-ref-default-bibliography '("C:/Users/rache/Google Drive/zotero.bib"))
(setq bibtex-completion-bibliography "C:/Users/rache/Google Drive/zotero.bib")
;(setq orb-insert-interface 'helm-bibtex)
(global-set-key (kbd "<f6>") #'org-ref-helm-insert-cite-link)

;; Org-roam-bibtex
(require `org-roam-bibtex)
(add-hook 'after-init-hook #'org-roam-bibtex-mode)
(define-key org-roam-bibtex-mode-map (kbd "C-c n a") #'orb-note-actions)

; PDF-tools
(use-package pdf-tools)
(pdf-tools-install)
(use-package org-noter)

; Setup with ORB
(setq orb-preformat-keywords
   '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point)
         ""
         :file-name "${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n#+roam_tags: source

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: ${file}
:NOTER_PAGE:
:END:")))

;;; Open PDF file stored in Windows' standard Zotero storage
(setq bibtex-completion-pdf-field "File")

