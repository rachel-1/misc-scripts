;; Org and Org-roam - https://github.com/nobiot/Zero-to-Emacs-and-Org-roam

; Point to GCC installed under MSYS2 for Windows to compile emacs-sqlite.
(cond ((eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/msys64/mingw64/bin")
))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename (cond ((eq system-type 'windows-nt)
       (setq org-directory "G:/My Drive/Org")
       )
      ((eq system-type 'gnu/linux)
       (setq org-directory "~/org")       
       )
      )
   ))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-x C-b" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (make-directory org-roam-directory :parents)
  (make-directory (concat (file-name-as-directory org-roam-directory) "personal") :parents)
  (make-directory (concat (file-name-as-directory org-roam-directory) "aurora") :parents)
  (make-directory (concat (file-name-as-directory org-roam-directory) "shared") :parents)
  (org-roam-db-autosync-mode))

;; Acknowledge migration to v2.
(setq org-roam-v2-ack t)

;;; Set up templates
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :if-new (file+head "personal/${slug}.org"
                            "#+title: ${title}\n\n")
         :unnarrowed t)
        ("z" "project" plain "%?"
         :if-new (file+head "shared/projects/${slug}.org"
                            "#+STARTUP: content\n#+title: ${title}\n\n")
         :unnarrowed t)
        ("a" "aurora notes" plain "%?"
         :if-new (file+head "aurora/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :aurora:\n")
         :unnarrowed t)
        ("m" "aurora meeting" plain "* ${title} %t\n%?"
         :if-new (file+head "aurora/meetings.org"
                            "#+title: Aurora Meetings\n#+filetags: :aurora:\n")
         :clock-in t :clock-resume t :kill-buffer t
         )        
        ("i" "introspection" plain "%?"
         :if-new (file+head "personal/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :introspection:\n")
         :unnarrowed t)
        ("b" "book" plain "%?"
         :if-new (file+head "personal/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :book:\n[rating] [genre] ([format]) from [recommender]\n\n* Review\n\n* Purpose\n\n* Main Ideas\n* Reflections\n* Action Items\n")
         :unnarrowed t)
        ("t" "task" plain "%?"
         :if-new (file+head "shared/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :task:\n")
         :unnarrowed t)
        ("s" "shared" plain "%?"
         :if-new (file+head "shared/${slug}.org"
                            "#+title: ${title}\n")
         :unnarrowed t)
        ("e" "event" plain "%?"
         :if-new (file+head "personal/<%<%Y-%m-%d>>_${slug}.org"
                            "#+title: ${title}\n#+file_tags: :event:\n")
         :unnarrowed t)
        ("p" "person" plain "%?"
         :if-new (file+head "personal/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :person:\n")
         :unnarrowed t)
        ("l" "location" plain "%?"
         :if-new (file+head "personal/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :location:\n")
         :unnarrowed t)
        ("c" "company" plain "%?"
         :if-new (file+head "personal/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :company:\n")
         :unnarrowed t)
        ("r" "bibliography reference" plain
         "- tags ::
- keywords :: %^{keywords}

* %^{title}
:PROPERTIES:
:Custom_ID: %^{citekey}
:URL: %^{url}
:AUTHOR: %^{author-or-editor}
:NOTER_DOCUMENT: %^{file}  
:NOTER_PAGE:  
:END:"
         :if-new
         (file+head "shared/papers/${citekey}.org" "#+title: ${title}\n")
         :unnarrowed t)
        )
      )

;;; Set up daily notes
(setq org-roam-dailies-directory "shared/journal/")

(setq org-roam-dailies-capture-templates
    '(("d" "default" entry
    "* %?"
    :if-new (file+head "%<%Y-%m-%d>.org"
                       "#+title: %<%Y-%m-%d (%a)>\n#+roam_tags:journal\n\n"))
      ("w" "week" plain
       "* %?"
       :if-new (file+head "weekly_review_%<%Y-%m-%d>.org"
                          "#+title: Week of %t\n#+roam_tags:journal\n

* Preparation
** Goals

* Reflection: Get Clear
** Process all inbound [0/5]
- [ ] personal email
- [ ] work email
- [ ] texts
- [ ] tabs
- [ ] receipts
** Tidy up [0/2]
- [ ] desk
- [ ] tasks
** Empty my head [0/1]
- [ ] Write whatever is on my mind: new projects, tasks, waiting-fors, goals, etc.

* Reflection: Time Spent
#+BEGIN: clocktable :scope agenda :maxlevel 2 :link t :fileskip0 t :compact t :narrow 50 :block thisweek
#+END:

#+begin_src elisp
(org-roam-dailies-copy-week-of-entries)
#+end_src

* Reflection
/What's on my mind? What went well and what didn't? What did I do that will prepare me for my  [[file:20210627154021-goals.org][goals]]?/ What can I add to my learnings (in files like [[id:fe7ecd3a-a174-42b6-b914-0f1aae38c550][Aurora Learnings (Meta)]])?"))
      ))

(global-set-key (kbd "C-c j") #'org-roam-dailies-goto-today)

(defun org-roam-dailies-copy-week-of-entries ()
  "Copy in the previous week of daily notes into a single buffer for easier viewing."
  (interactive)
  ; Set up the directory to point to our daily journal notes (in format Y-m-d.org)
  (setq dirname (concat (file-name-as-directory org-directory) org-roam-dailies-directory))
  ; Parse the current time into day/month/year
  (setq datetime (decode-time))
  (setq day (nth 3 datetime))
  (setq month (nth 4 datetime))
  (setq year (nth 5 datetime))
  ; Loop 7 times, going backward one day at a time
  (setq num 0)
  (while (< num 7)
    ; Create a heading for the next file
    (setq time (encode-time 1 1 0 day month year))
    (setq date (format-time-string "%Y-%m-%d" time))
    (insert "* "date"\t:auto:\n")
    ; Load the contents of the file, skipping it if it doesn't exist
    (ignore-errors (insert-file-contents (concat (file-name-as-directory dirname) date ".org") nil))
    ; Demote all headings so it will fit under the new heading we just made
    (org-map-entries #'org-demote-subtree "+LEVEL=1-auto" 'nil)
    ; Move the cursor to the end of the buffer
    (end-of-buffer)
    ; Move backward one day, but increment our list counter
    (setq day (1- day))
    (setq num (1+ num))
    )
  )

(global-set-key (kbd "C-c w") #'org-roam-dailies-copy-week-of-entries)

;; Taken from https://github.com/org-roam/org-roam/wiki/Hitchhiker's-Rough-Guide-to-Org-roam-V2
(defun org-hide-properties ()
  "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
      (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
        (overlay-put ov_this 'display "")
        (overlay-put ov_this 'hidden-prop-drawer t))))
  (put 'org-toggle-properties-hide-state 'state 'hidden))

(defun org-show-properties ()
  "Show all org-mode property drawers hidden by org-hide-properties."
  (interactive)
  (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
  (put 'org-toggle-properties-hide-state 'state 'shown))

(defun org-toggle-properties ()
  "Toggle visibility of property drawers."
  (interactive)
  (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
      (org-show-properties)
    (org-hide-properties)))
(add-hook 'org-mode-hook 'org-hide-properties)
