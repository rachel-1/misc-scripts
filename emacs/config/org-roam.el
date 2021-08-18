;; Org and Org-roam - https://github.com/nobiot/Zero-to-Emacs-and-Org-roam

; Point to GCC installed under MSYS2 for Windows to compile emacs-sqlite.
(cond ((eq system-type 'windows-nt)
  (add-to-list 'exec-path "C:/msys64/mingw64/bin")
))

(use-package org-roam)

;; Acknowledge migration to v2.
(setq org-roam-v2-ack t)

;;; Pick a new base directory for org roam
(setq org-roam-directory org-directory)

;;; Tell Emacs to start org-roam-mode when Emacs starts
(add-hook 'after-init-hook 'org-roam-setup)

;;; Define key bindings for Org-roam
(global-set-key (kbd "C-c n r") #'org-roam-toggle)
(global-set-key (kbd "C-c n i") #'org-roam-node-insert)
(global-set-key (kbd "C-x C-b") #'org-roam-node-find)

;;; Set up templates
(make-directory org-directory :parents)
(make-directory (concat (file-name-as-directory org-directory) "personal") :parents)
(make-directory (concat (file-name-as-directory org-directory) "aurora") :parents)
(setq org-roam-capture-templates
      '(("d" "default" plain "%?"
         :if-new (file+head "personal/${slug}.org"
                            "#+title: ${title}\n\n")
         :unnarrowed t)
        ("a" "aurora confidential" plain "%?"
         :if-new (file+head "aurora/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :aurora:\n")
         :unnarrowed t)
        ("i" "introspection" plain "%?"
         :if-new (file+head "personal/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :introspection:\n")
         :unnarrowed t)
        ("b" "book" plain "%?"
         :if-new (file+head "personal/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :book:\n[rating] [genre] ([format]) from [recommender]\n\n* Review\n\n* Purpose\n\n* Main Ideas\n* Reflections\n* Action Items\n")
         :unnarrowed t)
        ("t" "task" plain "%?"
         :if-new (file+head "personal/${slug}.org"
                            "#+title: ${title}\n#+file_tags: :task:\n")
         :unnarrowed t)
        ("e" "event" plain "%?"
         :if-new (file+head "personal/${slug}.org"
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
        ))

;;; Set up daily notes
(setq org-roam-dailies-directory "personal/daily/")

(setq org-roam-dailies-capture-templates
    '(("d" "default" entry
    "* %?"
    :if-new (file+head "%<%Y-%m-%d>.org"
                       "#+title: %<%Y-%m-%d (%a)>\n#+roam_tags:journal\n* Meetings\n\n* Readings\n\n* Work Log\n#+BEGIN: clocktable :scope agenda :maxlevel 3 :block today :link t :fileskip0 t :hidefiles t :compact t :narrow 100
#+END:
\n* Reflections"))
      ("w" "week" entry
       "* %?"
       :if-new (file+head "%<%Y-%m-%d>"
                          "#+title: %<%Y-%m-%d>\n#+roam_tags:journal\n* * Checklist
** Get clear
*** Process all inbound [0/4]
- [ ] emails
- [ ] texts
- [ ] tabs
- [ ] receipts
*** Tidy up [0/2]
- [ ] desk
- [ ] tasks
*** Empty my head [0/1]
  /Write whatever is on my mind: new projects, tasks, waiting-fors, goals, etc./
  - [ ] see Reflections below
** Get current
*** Look over time spent [0/2]
/Write what went well and what didn't./
- [ ] ~org-roam-dailies-copy-week-of-entries~
- [ ] see Reflections below
*** Review upcoming events/tasks [0/2]
- [ ] review events of the week ahead
- [ ] remove/reprioritize old tasks
*** Revisit both personal [[file:20210627154021-goals.org][goals]] and project goals [0/1]
/Write about how I am or am not on track for the goals./
- [ ] see Reflections below
* Work Log\n
#+BEGIN: clocktable :scope agenda :maxlevel 3 :tstart '<today-7>' :tend '<now>' :link t :fileskip0 t :hidefiles t :compact t :narrow 100
#+END:
* Reflections
/What's on my mind? What went well and what didn't? What did I do that will prepare me for my long-term goals?/
\n* Reflections"))
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
