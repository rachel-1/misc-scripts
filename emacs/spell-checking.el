;; Spell checking

(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code.
       ;; TODO: make sure hunspell is installed
       (setenv "LANG" "en_US")
       (setq ispell-program-name
             "C:/Users/rache/bin/hunspell/bin/hunspell.exe")
       )
      ((eq system-type 'gnu/linux)
       ;; Linux-specific code goes here. 
       ))

(global-set-key (kbd "C-x c") #'helm-flyspell-correct)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
