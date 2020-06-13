
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

(global-set-key (kbd "<S-left>") 'windmove-left)
(global-set-key (kbd "<S-right>") 'windmove-right)
(global-set-key (kbd "<S-up>") 'windmove-up)
(global-set-key (kbd "<S-down>") 'windmove-down)

(after! haskell
  (setq haskell-interactive-popup-errors nil)
  (setq haskell-process-suggest-remove-import-lines nil)
  (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-w3m-open-haddock)

  (define-key haskell-mode-map (kbd "C-x C-s")
    (lambda ()
      (interactive)
      (save-excursion
        (beginning-of-buffer)
        ;; Sort the initial import block
        (when (search-forward "import" nil t)
          (beginning-of-line)
          (haskell-sort-imports)
          )

        ;; Sort any following import blocks
        (while (search-forward "

import" nil t)
          (beginning-of-line)
          (haskell-sort-imports)
          ))

      (save-buffer)))
  )

(after! smartparens
  (load "/home/tom/doom-config/my-smartparens.el")
  )
;; (add-to-list 'completion-styles 'flex)
;; (setq completion-styles '(flex))

;; (setq helm-completion-style 'helm-fuzzy)

(global-set-key (kbd "M-z") 'zap-up-to-char)

;; Put some Doom defaults back to normal
(global-set-key (kbd "C-_") 'undo)
(global-set-key (kbd "C-a") 'beginning-of-line)

(global-visual-line-mode)

(after! javascript
  (setq typescript-indent-level 2)
  )

;; (speedbar-add-supported-extension ".hs")

(after! helm
  ;; Helm buffer sort order is crazy without this; see
  ;; https://github.com/emacs-helm/helm/issues/1492
  (defun helm-buffers-sort-transformer@donot-sort (_ candidates _) candidates)
  (advice-add 'helm-buffers-sort-transformer :around 'helm-buffers-sort-transformer@donot-sort)
  )
