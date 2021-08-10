;; Org and Org-roam - https://github.com/nobiot/Zero-to-Emacs-and-Org-roam

;;; Tell Emacs where sqlite3.exe is stored
(add-to-list 'exec-path "c:/Users/rache/bin/sqlite-tools-win32-x86-3360000")

;;; Pick a new base directory for org roam
(setq org-roam-directory "c:/Users/rache/Google Drive/Org")

;;; Tell Emacs to start org-roam-mode when Emacs starts
(add-hook 'after-init-hook 'org-roam-mode)

;;; Define key bindings for Org-roam
(global-set-key (kbd "C-c n r") #'org-roam-buffer-toggle-display)
(global-set-key (kbd "C-c n i") #'org-roam-insert)
;(global-set-key (kbd "C-c n /") #'org-roam-find-file)
(global-set-key (kbd "C-x C-b") #'org-roam-find-file)
(global-set-key (kbd "C-c n b") #'org-roam-switch-to-buffer)
(global-set-key (kbd "C-c n d") #'org-roam-find-directory)

;;; Recommendation for Windows users for performance
;;; https://github.com/org-roam/org-roam/issues/1289#issuecomment-744046148
(setq org-roam-db-update-method 'immediate)


;;; Set up daily notes
(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
    '(("d" "default" entry
    #'org-roam-capture--get-point
    "* %?"
    :file-name "daily/%<%Y-%m-%d>"
    :head "#+title: %<%Y-%m-%d (%a)>\n#+roam_tags:journal\n* Meetings\n\n* Readings\n\n* Work Log\n#+BEGIN: clocktable :scope agenda :maxlevel 3 :block today :link t :fileskip0 t :hidefiles t :compact t :narrow 100
#+END:
\n* Reflections")
      ("w" "week" entry
    #'org-roam-capture--get-point
    "* %?"
    :file-name "daily/%<%Y-%m-%d>"
    :head "#+title: %<%Y-%m-%d>\n#+roam_tags:journal\n* * Checklist
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
\n* Reflections")
))

(global-set-key (kbd "C-c j") #'org-roam-dailies-find-today)

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
