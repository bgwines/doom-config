
;; (setq doom-theme 'doom-tomorrow-night)
(setq doom-theme 'afternoon)

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))
(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(90 90))))

;; Help
(global-set-key (kbd "C-?") 'help-command)

;; Editing
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; ace-jump-mode
(global-set-key (kbd "C-0") 'ace-jump-mode)
(global-set-key (kbd "C-9") 'ace-jump-line-mode)

;; Scratch
(defun new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (switch-to-buffer (loop for num from 0
                          for name = (format "scratch-%03i" num)
                          while (get-buffer name)
                          finally return name)))
(global-set-key (kbd "<f7>") 'new-scratch)

;; Multiple cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; magit
(global-set-key (kbd "<f8>") 'magit-status)

;; next-error and previous-error
(global-set-key (kbd "C-x C-n") 'flycheck-next-error)
(global-set-key (kbd "C-x C-p") 'flycheck-previous-error)

;; modes
(whole-line-or-region-mode)
(global-subword-mode)

;; helm-swoop
(global-set-key (kbd "C-c o") 'helm-swoop)

;; projectile-helm-ag
(defun projectile-helm-ag (arg)
  "Run helm-do-ag relative to the project root.  Or, with prefix arg ARG, relative to the current directory."
  (interactive "P")
  (if arg
      (progn
        ;; Have to kill the prefix arg so it doesn't get forwarded
        ;; and screw up helm-do-ag
        (set-variable 'current-prefix-arg nil)

        (if dired-directory
            (helm-do-ag dired-directory)
          (helm-do-ag (file-name-directory (buffer-file-name)))
          )
        )
    (helm-do-ag (projectile-project-root))
    ))
(global-set-key (kbd "C-x C-r") 'projectile-helm-ag)

;; Disable line numbers
(setq display-line-numbers-type nil)

;; expand-region
(global-set-key "\M-s" 'er/expand-region)

;; projectile-find-file
(global-set-key (kbd "C-t") 'projectile-find-file)

;; projectile-find-file
(global-set-key (kbd "M-;") 'whole-line-or-region-comment-dwim)

(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; iflipb
(global-set-key (kbd "M-j") 'iflipb-next-buffer)
(global-set-key (kbd "M-k") 'iflipb-previous-buffer)

(setq company-bg-color (face-attribute 'default :background))

(custom-set-faces
 '(helm-selection ((t :background "gray25" :distant-foreground "black" :foreground "white smoke")))
 )
