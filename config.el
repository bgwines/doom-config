
;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "<tab>") 'indent-according-to-mode)

;;;;;;;;;;;;;
;; buffers ;;
;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-b") 'ibuffer)
;;(setq ibuffer-saved-filter-groups
;;      '(("home"
;;         ("emacs" (or (filename . ".emacs.d")
;;                      (filename . ".spacemacs")))
;;                      (filename . "doom-emacs")))
;;         ("quip/services" (filename . "quip/services"))
;;         ("quip/templates" (filename . "quip/templates"))
;;         ("quip/lib" (filename . "quip/lib"))
;;         ("quip/data" (filename . "quip/data"))
;;         ("quip/proto" (filename . "quip/proto"))
;;         ("Magit" (name . "\*magit"))
;;         ("Help" (or (name . "\*Help\*")
;;                     (name . "\*Apropos\*")
;;                     (name . "\*info\*"))))))
;;(add-hook 'ibuffer-mode-hook
;;          '(lambda ()
;;             (ibuffer-switch-to-saved-filter-groups "home")))
;; remapping lost keystrokes
(after! ibuffer
  (define-key ibuffer-mode-map (kbd "<tab>") 'ibuffer-forward-filter-group))

;; splitting panes (same commands as tmux)
(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "C-x |") 'split-window-right)
(global-set-key (kbd "C-x -") 'split-window-below)
(global-set-key (kbd "C-x y") 'delete-window)

;;;;;;;;;;;
;; magit ;;
;;;;;;;;;;;

(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)

;;;;;;;;;;;;;
;; editing ;;
;;;;;;;;;;;;;

;; undo
(global-set-key (kbd "M-_") 'undo-tree-redo)

(defun new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (switch-to-buffer (loop for num from 0
                          for name = (format "scratch-%03i" num)
                          while (get-buffer name)
                          finally return name)))

;; cursor navigation
(backward-forward-mode t)
;;(global-set-key (kbd "C-<left>") 'pop-tag-mark)
;;(global-set-key (kbd "C-<right>") 'pop-tag-mark)
;;(define-key smartparens-mode-map (kbd "C-<left>") 'pop-tag-mark)
;;(define-key smartparens-mode-map (kbd "C-<right>") 'pop-tag-mark)

;; jump to start of line
(global-set-key (kbd "C-'") 'back-to-indentation)

;; move cursor to top and bottom of window, set to M-l for symmetry with C-l
(global-set-key (kbd "M-l") 'move-to-window-line-top-bottom)

;; something-to-char
(defvaralias 'lazy-highlight-face 'isearch-lazy-highlight) ;; NOT sure why
    ;; this fixes a thing
    ;; but it definitely fixes something with the next two commands

(defun zap-backwards-to-char (arg char)
  (interactive "p\ncZap backwards to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (kill-region (point) (progn
                         (search-backward (char-to-string char)
                                         nil nil arg)
                         (forward-char)
                         (point))))
(global-set-key (kbd "M-;") 'zap-backwards-to-char)
(global-set-key (kbd "M-:") 'fastnav-mark-to-char-backward)
(global-set-key (kbd "M-Z") 'fastnav-mark-to-char-forward)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; comment out region/line
(global-set-key (kbd "C-/") 'whole-line-or-region-comment-dwim)

;; avy
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-char-2)

;; multiple cursors
(global-set-key (kbd "M-u") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-h") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(setq mc/always-repeat-command t)
(setq mc/always-run-for-all t)

;; expand-region
(global-set-key (kbd "M-a") 'er/contract-region)
(global-set-key (kbd "M-s") 'er/expand-region)

;; join-line
(global-set-key (kbd "C-j") 'join-line)

;;;;;;;;;;;;;;
;; deletion ;;
;;;;;;;;;;;;;;
;; backwards
(global-set-key (kbd "C-i") 'delete-backward-char)
(global-set-key (kbd "M-i") 'subword-backward-kill)
(global-set-key (kbd "M-I") 'backward-kill-sexp)

(defun subword-backward-delete ()
  (interactive)
  (subword-backward-kill 1)
  (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer))
  )

(after! helm-files
  (define-key helm-map (kbd "C-i") 'delete-backward-char)
  (define-key helm-map (kbd "M-i") 'subword-backward-delete)
  (define-key helm-find-files-map (kbd "C-i") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "M-i") 'subword-backward-delete)
  (define-key helm-find-files-map (kbd "C-k") 'kill-line-no-fill-kill-ring)
  )
(after! isearch
  (define-key isearch-mode-map (kbd "C-i") 'isearch-del-char))
(global-set-key (kbd "M-k") 'kill-whole-line)

;; moving a line
(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; spaces, not tabs
(setq-default indent-tabs-mode nil)

;; Always add newline at end of file.
(setq require-final-newline t)

;;;;;;;;;;;;;;;
;; cosmetics ;;
;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (setq doom-theme 'doom-tomorrow-night)
(setq doom-theme 'afternoon)
(setq doom-font (font-spec :family "Source Code Pro" :size 18))

;; dim inactive panes
(defun highlight-selected-window ()
  "Highlight selected window with a different background color."
  (walk-windows (lambda (w)
                  (unless (eq w (selected-window))
                    (with-current-buffer (window-buffer w)
                      (buffer-face-set '(:background "#333"))))))
  (buffer-face-set 'default))
(add-hook 'buffer-list-update-hook 'highlight-selected-window)
;;(add-hook 'after-init-hook (lambda ()
;;  (when (fboundp 'auto-dim-other-buffers-mode)
;;    (auto-dim-other-buffers-mode t))))

(after! company
 (setq company-bg-color (face-attribute 'default :background))

 (custom-set-faces
  '(company-preview-common ((t (:background "#21e824bc35b0"))))
  '(company-scrollbar-bg ((t (:background "#2bd12f784561"))))
  '(company-scrollbar-fg ((t (:background "#21e824bc35b0"))))
  '(company-tooltip ((t (:inherit default :background "#1bf61e4b2c46"))))
  '(company-tooltip-annotation ((t (:foreground "deep sky blue"))))
  '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :foreground "deep sky blue" :weight bold))))
  '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
  '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
  )
 )
(after! helm
 ;; Default green selection color is hideous
 (custom-set-faces
  '(helm-selection ((t :background "gray25" :distant-foreground "black" :foreground "white smoke")))
  )
 )

;; cursor
(setq-default cursor-type 'bar)
(after! hl-line
  (set-face-background hl-line-face "#333333")
  (set-face-attribute hl-line-face nil :underline t))

;; When M-q formatting a comment, only use one space instead of two
;; after a period.
(set-variable 'sentence-end-double-space nil)

;; 80 char limit
(after! c++
  (add-hook 'c++-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow))))
(after! java
  (add-hook 'java-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{101\\}" 'hi-yellow))))
(after! js
  (add-hook 'js-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow))))
(after! jsx
  (add-hook 'jsx-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow))))
(after! protobuf
  (add-hook 'protobuf-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow))))
(after! python
  (add-hook 'python-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow))))

;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;

(after! helm
  ;; Helm buffer sort order is crazy without this; see
  ;; https://github.com/emacs-helm/helm/issues/1492
  (defun helm-buffers-sort-transformer@donot-sort (_ candidates _) candidates)
  (advice-add 'helm-buffers-sort-transformer :around 'helm-buffers-sort-transformer@donot-sort)
  )

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

;; helm-swoop
(global-set-key (kbd "C-M-s") 'helm-swoop)
(setq helm-swoop-use-line-number-face nil)

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
;; which of these is better? The former, right?
(global-set-key (kbd "C-x C-r") 'projectile-helm-ag)
(global-set-key (kbd "M-g M-r") 'helm-projectile-ag)

;; (global-set-key (kbd "C-t") 'projectile-find-file) (Tom uses this)
(global-set-key (kbd "C-t") 'helm-projectile-find-file)
(setq helm-locate-command
      "glocate %s %s"
      helm-locate-create-db-command
      "gupdatedb --output='%s' --localpaths='%s'")

;; projectile
(setq projectile-project-search-path '("~/quip"))
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)

;; dumb-jump
;;(setq dumb-jump-default-project "~/quip")
;;(global-set-key (kbd "M-.") 'dumb-jump-go)
;;(setq dumb-jump-force-searcher 'ag)

;; Makes duplicate files show up as application.py|api instead of the <2>.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; follow symlinks
(setq-default vs-follow-symlinks t)
(setq vc-follow-symlinks t)

;;;;;;;;;;;;
;; saving ;;
;;;;;;;;;;;;

;; Automatically revert all buffers when files change on disk, e.g.
;; after a git pull, git rebase, or python autoimports
(global-auto-revert-mode t)

(setq auto-save-default nil)

;; trim whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; automatically save buffers associated with files on buffer switch
;; and on windows switch
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-up (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-down (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-left (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice windmove-right (before other-window-now activate)
  (when buffer-file-name (save-buffer)))

;;;;;;;;;;;;;;
;; assorted ;;
;;;;;;;;;;;;;;

;; yapf (don't auto-perform this upon save since it locks up emacs for a sec)
(defun yapf-current-buffer ()
  (interactive)
  (shell-command-to-string
   (format "~/quip/bin/yapf --inplace %s" buffer-file-name)))
(global-set-key (kbd "C-M-y") 'yapf-current-buffer)

;; rename-file
(defun rename-file-and-buffer (new-name)
"Renames both current buffer and file it's visiting to NEW-NAME."
(interactive "sNew name: ")
(let ((name (buffer-name))
  (filename (buffer-file-name)))
  (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
    (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;

(global-subword-mode)
(global-visual-line-mode)
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;
;; smartparens ;;
;;;;;;;;;;;;;;;;;

(after! smartparens
  (load "~/doom-config/my-smartparens.el")
  )

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(after! yasnippet
  (yas-load-directory "~/.emacs.d/private/snippets")
)

;; -----------------
;; Tom's stuff below
;; -----------------

(setq doom-modeline-buffer-file-name-style 'truncat?rre-with-project)
