;;; Inline tasks
;(require 'org-inlinetask) TODO

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(d)" "|" "DONE(f)")
        (sequence "BLOCKED(b)" "|" "DONE(f)")))

(cond ((eq system-type 'windows-nt)
       (setq org-directory "c:/Users/rache/Google Drive/Org")
       )
      ((eq system-type 'gnu/linux)
       (setq org-directory "~/org")       
       )
      )
        
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-default-notes-file (concat (file-name-as-directory org-directory) "todo.org"))

;; Set up searching
(setq deft-directory org-directory)

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

(global-set-key "\C-cs" 'my-org-screenshot)

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
(global-set-key (kbd "C-o") #'org-peek-link-at-point)

;; Have org-mode prompt to handle idle (i.e. non-Emacs time) properly
(setq org-clock-idle-time 5)

;; Set up pomodoro timer
(setq org-pomodoro-format "Work~%s")
(setq org-pomodoro-keep-killed-pomodoro-time t)
(setq org-pomodoro-manual-break t)
(setq org-pomodoro-audio-player "mplayer")
(setq org-pomodoro-finished-sound-args "-volume 0.3")
(setq org-pomodoro-long-break-sound-args "-volume 0.3")
(setq org-pomodoro-short-break-sound-args "-volume 0.3")
(global-set-key (kbd "C-c p") #'org-pomodoro)

;; ; The org-bullets package replaces all headline markers with different Unicode bullets:
(use-package org-bullets
      :config
      (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
 (setq org-bullets-bullet-list '("◉" "○" "•" "-"))
