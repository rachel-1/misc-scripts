;;; Inline tasks
;(require 'org-inlinetask) TODO

(setq   org-highest-priority ?A
        org-default-priority ?C
        org-lowest-priority ?D
)

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(d)" "|" "DONE(f)" "ABANDONED(a)")
        (sequence "BLOCKED(b)" "|" "DONE(f)")
        ))

(cond ((eq system-type 'windows-nt)
       (setq org-directory "G:/My Drive/Org")
       (setq org-agenda-files '("G:/My Drive/Org/shared/projects"))
       )
      ((eq system-type 'gnu/linux)
       (setq org-directory "~/org")
       (setq org-agenda-files '("~/org/shared/projects/"))       
       )
      )

(setq org-agenda-custom-commands
      '(("w" "Work tasks"
         ((tags-todo "aurora")))
        ))

(defun org-agenda-show-work-tasks (&optional arg)
  (interactive "P")
  (setq org-agenda-hide-tags-regexp (regexp-opt '("project" "aurora")))
  (org-agenda arg "w"))
(global-set-key (kbd "C-c a") 'org-agenda-show-work-tasks)


(setq org-refile-targets '((nil :maxlevel . 1)))

(global-set-key (kbd "C-c c") #'org-capture)
(setq org-default-notes-file (concat (file-name-as-directory org-directory) "todo.org"))

;; Set up searching
(use-package deft)
(setq deft-extensions '("org"))
(setq deft-directory org-directory)
(setq deft-recursive t)

(setq org-tag-alist '(("review_feedback" . ?r) ("pr" . ?p) ("learning" . ?l)))


;; Allow copy-paste to Slack
(use-package ox-slack)
;; Allow copy-paste to Google Docs
(use-package ox-clip)

;; Allow quick insertion of certain templates
(setq org-structure-template-alist
      '(("b" . "src bash\n")))
(global-set-key (kbd "C-x i") 'org-insert-structure-template)

;;; Take screenshots to insert:
; https://www.sastibe.de/2018/11/take-screenshots-straight-into-org-files-in-emacs-on-win10/
(defun my-org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (buffer-file-name)
                  "_"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (shell-command "snippingtool /clip")
  (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
  (insert (concat "[[file:" filename "]]"))
  (org-display-inline-images))

;(global-set-key "\C-cs" 'my-org-screenshot)
(use-package org-attach-screenshot)
(global-set-key (kbd "C-c s") 'org-attach-screenshot)

; https://zzamboni.org/post/beautifying-org-mode-in-emacs/
; Hide /.../ for italics, *...* for bold, etc.
(setq org-hide-emphasis-markers t)
; Then, we set up a font-lock substitution for list markers (I always use “-” for lists, but you can change this if you want) by replacing them with a centered-dot character:
(font-lock-add-keywords 'org-mode
                         '(("^ *\\([-]\\) "
                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; TODO
;;(setq org-roam-encrypt-files t)
;;(custom-set-variables
;; '(epg-gpg-home-directory "")
;; '(epg-gpg-program "")
;; '(epg-gpgconf-program "")
;;)

 (defun insert-quote ()
   "Allow the user to copy text from another org file, inserting a link to the specific region in the other file."
   (interactive)
   (setq curr-point (mark-marker))
   (set-window-dedicated-p (selected-window) t)
   (org-roam-find-file)
   (setq title
         (nth 1 (nth 0
              (org-collect-keywords '("TITLE"))
              )))
   (set-window-dedicated-p (selected-window) nil)
   (setq quote (read-string "Enter quote: "))
   (setq id-val (org-id-get-create))
   (delete-window)
   (goto-char curr-point)
   (insert "\"" quote "\" " "([[id:" id-val "][" title "]])" )
   (set-window-dedicated-p (selected-window) nil) 
 )
  
(global-set-key (kbd "C-c q") #'insert-quote)

; Inspired by https://emacs.stackexchange.com/questions/7409/is-there-a-generic-toggle-previous-window-function
(defun org-peek-link-at-point ()
   "Allow the user to peek at the target of an Org link without switching buffers. Specifically, opens the link like normal, then returns to focus on the previous window."
   (interactive)
   (org-open-at-point)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win)))
 )

(global-set-key (kbd "C-c C-o") #'org-open-at-point)
(global-set-key (kbd "C-x C-d") #'org-table-delete-column)
(global-set-key (kbd "C-o") #'org-peek-link-at-point)

(bind-key* "C-c C-x C-j" 'org-clock-goto)
(global-set-key (kbd "C-c i") #'org-clock-in)
(bind-key* "C-c C-i" (lambda () (interactive) (let ((current-prefix-arg '(4))) (call-interactively #'org-clock-in))))

;; Keep clock history between restarts. This lets us C-c C-i to clock into interrupted tasks.
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; Set up pomodoro timer
(use-package org-pomodoro)
(setq org-pomodoro-format "Work~%s")
(setq org-pomodoro-keep-killed-pomodoro-time t)
(setq org-pomodoro-manual-break t)
(setq org-pomodoro-finished-sound-p nil)
(setq org-pomodoro-long-break-sound-p nil)
(setq org-pomodoro-overtime-sound-p nil)
(setq org-pomodoro-short-break-sound-p nil)
;; If you want to enable audio again, you will likely want to use commands like
;; these to reduce the volume.
;(setq org-pomodoro-audio-player "mplayer")
;(setq org-pomodoro-finished-sound-args "-volume 0.3")
;(setq org-pomodoro-long-break-sound-args "-volume 0.3")
;(setq org-pomodoro-short-break-sound-args "-volume 0.3")
(global-set-key (kbd "C-c C-p") #'org-pomodoro)

;; Don't round time durations to days.
(setq org-duration-format 'h:mm)

;; ; The org-bullets package replaces all headline markers with different Unicode bullets:
(use-package org-bullets
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
 (setq org-bullets-bullet-list '("◉" "○" "•" "-"))

(setq org-clocktable-defaults '(:maxlevel 2 :lang "en" :scope file :block nil :wstart 1 :mstart 1 :tstart nil :tend nil :step nil :stepskip0 nil :fileskip0 t :tags nil :match nil :emphasize nil :link nil :narrow 40! :indent t :formula nil :timestamp nil :level nil :tcolumns nil :formatter nil))

;; Create global hotkeys to jump to certain bookmarks
(bind-key* "C-j q" (lambda () (interactive) (bookmark-jump "questions")))
(bind-key* "C-j i" (lambda () (interactive) (bookmark-jump "issuesets")))
(bind-key* "C-j r" (lambda () (interactive) (bookmark-jump "reportability")))
(bind-key* "C-j m" (lambda () (interactive) (bookmark-jump "mlab")))
(bind-key* "C-j p" (lambda () (interactive) (bookmark-jump "process")))

;; Allow bash execution of code blocks
(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)
(setq org-confirm-babel-evaluate nil)

(defun create-code-todo ()
  (interactive)
  (setq task-name (read-string "Enter name of task: "))
  (insert (concat "* TODO " task-name "\n"))
  (setq branch-name (concat "rachel-1/" (downcase (replace-regexp-in-string " " "_" task-name))))
  (insert (concat ":PROPERTIES:\n:BRANCH: " branch-name "\n"))
  (insert (concat ":PR: N/A\n:END:\n"))
  (setq current-buf (current-buffer))
  (if (= (length (window-list)) 1) (split-window-below))
  (other-window 1)
  (switch-to-buffer current-buf)
  (magit-status-av-repo)
  (if (y-or-n-p (concat "Create branch " branch-name " off master?"))
      (magit-branch-and-checkout branch-name "master")
      (kill-new branch-name)
      )
  )

(bind-key* "C-c C-x C-c" (lambda () (interactive) (create-code-todo)))

(defun checkout-branch ()
  (interactive)
  (org-previous-visible-heading 1)
  (setq branch (org-element-property :BRANCH (org-element-at-point)))
  (message "branch is %s" branch)
  (shell-command (concat "cd ~/av && git checkout " branch))
)

(defun show-pr-link ()
  (interactive)
  (org-previous-visible-heading 1)
  (setq pr-link (org-element-property :PR (org-element-at-point)))
  (kill-new pr-link)
  (message pr-link)
)

(defun org-archive-done (&optional arg)
  (org-todo 'done))

(advice-add 'org-archive-subtree :before 'org-archive-done)
(bind-key* "C-c >" (lambda () (interactive) (org-demote-subtree)))

(global-set-key (kbd "C-c l") #'org-store-link)
