(defun load-helper-file (filename)
  (load-file (format "~/.config/doom/helpers/%s" filename)))

;;;;;;;;;;;;;;;;;
;; major modes ;;
;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.tftpl\\'" . terraform-mode))
(add-to-list 'auto-mode-alist '("\\.hack\\'" . php-mode))

;;;;;;;;;;;;;;
;; SN Hydra ;;
;;;;;;;;;;;;;;

(require 'symbol-navigation-hydra)
(after! symbol-navigation-hydra
  (global-set-key (kbd "M-t") 'symbol-navigation-hydra-engage-hydra)
  (setq-default ahs-highlight-all-windows nil)
  (setq-default ahs-enable-focus-hooks nil)
  (setq-default ahs-highlight-upon-window-switch nil)
  (setq-default ahs-case-fold-search nil)
  (setq-default ahs-default-range 'ahs-range-whole-buffer)
  (setq-default ahs-idle-interval 999999999.0)
  (setq-default ahs-inhibit-face-list (delete 'font-lock-doc-string-face ahs-inhibit-face-list))
  (setq-default ahs-inhibit-face-list (delete 'font-lock-string-face ahs-inhibit-face-list))
  (setq-default ahs-inhibit-face-list (delete 'font-lock-doc-face ahs-inhibit-face-list))
  (setq-default symbol-navigation-hydra-project-search-fn
                '(lambda (q) (grr-helper "absolute-grr-server" q))))

;;;;;;;;;;;;;
;; buffers ;;
;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;
;; git ;;
;;;;;;;;;

(require 'git-link)
(after! git-link
  (setq-default git-link-use-commit t)
  (global-set-key (kbd "M-g M-l") 'git-link))


(defun magit-blame-get-github-url ()
  (let ((hash (oref (magit-current-blame-chunk) orig-rev)))
    (format "https://github.com/quip/quip/commit/%s" hash)))

(defun magit-blame-open-commit-in-github ()
  (interactive)
  (browse-url (magit-blame-get-github-url)))

(defun magit-blame-copy-commit-github-url ()
  (interactive)
  (progn
    (let ((url (magit-blame-get-github-url)))
      (kill-new url)
      (message url))))

(require 'magit)
(after! magit
  (define-key magit-blame-mode-map "o" 'magit-blame-open-commit-in-github)
  (define-key magit-blame-mode-map "w" 'magit-blame-copy-commit-github-url)
  (define-key magit-blame-mode-map "u" 'magit-blame-copy-commit-github-url)
  ;; undo the global-set-key
  ;;(define-key magit-mode-map "<tab>" 'magit-section-cycle)
  (setq magit-refresh-status-buffer nil)
  (global-set-key (kbd "M-g M-s") 'magit-status)
  (global-set-key (kbd "M-g M-b") 'magit-blame)
  ;;(global-set-key (kbd "M-g M-l") 'magit-log)
)


;;;;;;;;;;;;;
;; editing ;;
;;;;;;;;;;;;;

(load-helper-file "new-scratch.el")

;; rebind cmd-v away from kill-buffer
(global-set-key (kbd "s-k") 'yank)

;; faster up and down
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

;; undo
(global-set-key (kbd "M-_") 'undo-tree-redo)

;; line numbers
(global-display-line-numbers-mode 1)
(load-helper-file "goto-line.el")
(global-set-key (kbd "M-g M-g") 'hydra-goto-line/body)
(global-display-line-numbers-mode -1)
(setq display-line-numbers-type nil)

;; smartparens
(require 'smartparens)
(after! smartparens
  (load-helper-file "smartparens-hydra.el")
  (global-set-key (kbd "M-s") 'smartparens-hydra/body))

;; jump to start of line
(global-set-key (kbd "C-'") 'back-to-indentation)

;; move cursor to top and bottom of window, set to M-l for symmetry with C-l
(global-set-key (kbd "M-l") 'move-to-window-line-top-bottom)

;; mark or zap to char
(load-helper-file "mark-or-zap-to-chars.el")
(global-set-key (kbd "M-;") 'zap-backwards-to-char)
(global-set-key (kbd "M-:") 'mark-up-to-char-backward)
(global-set-key (kbd "M-Z") 'fastnav-mark-up-to-char-forward)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; multiple cursors
(global-set-key (kbd "M-u") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-h") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(setq mc/always-repeat-command t)
(setq mc/always-run-for-all t)

;; expand-region
(global-set-key (kbd "M-e") 'er/expand-region)

;;;;;;;;;;;;;;
;; deletion ;;
;;;;;;;;;;;;;;

(load-helper-file "deletion-functions.el")

(defun set-deletion-bindings (mode-map)
  (define-key mode-map (kbd "C-i") 'delete-backward-char)
  (define-key mode-map (kbd "M-i") 'subword-backward-delete)
  (define-key mode-map (kbd "C-k") 'delete-line-no-kill))

(global-set-key (kbd "C-i") 'delete-backward-char)
(global-set-key (kbd "M-i") 'subword-backward-kill)
(global-set-key (kbd "M-k") 'kill-whole-line)

(set-deletion-bindings minibuffer-local-map)
;;(after! scad-mode
;;  (set-deletion-bindings scad-mode-map))


(after! markdown-mode
  (set-deletion-bindings markdown-mode-map))

(after! helm-files
  (set-deletion-bindings helm-map)
  (set-deletion-bindings helm-read-file-map)
  (set-deletion-bindings helm-find-files-map))

(after! isearch
  (set-deletion-bindings isearch-mode-map))

;; moving a line
(require 'move-text)
(move-text-default-bindings)

;; spaces, not tabs
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "<tab>") 'indent-for-tab-command)

;; Always add newline at end of file.
(setq require-final-newline t)

;;;;;;;;;;;;;;;;
;; ace-window ;;
;;;;;;;;;;;;;;;;

(require 'ace-window)
(after! ace-window
  (load-helper-file "window-hydra.el")
  (global-set-key (kbd "C-,") 'window-hydra/body)
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s))
  (setq aw-dispatch-always t)
  (custom-set-faces
   '(aw-leading-char-face ((t :height 3.5 :foreground "deep sky blue" :inherit 'aw-leading-char-face))))
  (setq ace-window-display-mode t))

;; https://emacs.stackexchange.com/questions/17072/open-file-in-a-specific-window
(defun helm-buffer-run-ace-window ()
 (interactive)
 (with-helm-alive-p
   (helm-exit-and-execute-action 'helm-buffer-ace-window)))

(defun helm-file-run-ace-window ()
 (interactive)
 (with-helm-alive-p
   (helm-exit-and-execute-action 'helm-file-ace-window)))

(defun helm-file-ace-window (file)
  "Use ‘ace-window’ to select a window to display FILE."
  (unless (eq 1 (length (window-list)))
    (ace-select-window))
  (find-file file))

(defun helm-nonrelative-file-run-ace-window ()
 (interactive)
 (with-helm-alive-p
   (helm-exit-and-execute-action 'helm-nonrelative-file-ace-window)))

(defun helm-nonrelative-file-ace-window (grep-line)
  "Use ‘ace-window’ to select a window to display the file in GRR-LINE."
  (unless (eq 1 (length (window-list)))
    (ace-select-window))
  (let* ((prefix "~/quip/")
         (nonrelative-file (car (split-string grep-line ":")))
         (lineno (string-to-number (car (cdr (split-string grep-line ":")))))
         (file (if (string-prefix-p prefix nonrelative-file)
                   nonrelative-file
                 (concat prefix nonrelative-file))))
    (find-file file)
    (goto-line lineno)))

(setq helm-ace-command "M-RET")

(defun helm-buffer-ace-window (buffer)
  "Use ‘ace-window’ to select a window to display BUFFER."
  (unless (eq 1 (length (window-list)))
    (ace-select-window))
    (helm-window-show-buffers (list buffer)))

(after! helm-buffers
 (add-to-list 'helm-type-buffer-actions
              '((format "Switch to buffer in Ace window ‘%s'" helm-ace-command) .
                helm-buffer-ace-window)
              :append)
 (define-key helm-buffer-map (kbd helm-ace-command) #'helm-buffer-run-ace-window)
 )

(after! helm-projectile
 (add-to-list 'helm-projectile-file-actions
              '((format "Switch to file in Ace window ‘%s'" helm-ace-command) .
                helm-file-ace-window)
              :append)
 (define-key helm-projectile-find-file-map (kbd helm-ace-command) #'helm-file-run-ace-window)
 )

(after! helm-files
  (add-to-list 'helm-find-files-actions
               '((format "Switch to file in Ace window ‘%s'" helm-ace-command) .
                 helm-file-ace-window)
               :append)
  (add-to-list 'helm-type-file-actions
               '((format "Switch to file in Ace window ‘%s'" helm-ace-command) .
                 helm-file-ace-window)
               :append)

  (define-key helm-map (kbd helm-ace-command) #'helm-file-run-ace-window)
  (define-key help-mode-map (kbd helm-ace-command) #'helm-file-run-ace-window)
  (define-key helm-find-files-map (kbd helm-ace-command) #'helm-file-run-ace-window)
  )

;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;

(after! (:and helm helm-buffers)
 (setq! helm-buffers-sort-fn #'helm-fuzzy-matching-sort-fn-preserve-ties-order))

(after! helm
 ;; Default green selection color is hideous
 (custom-set-faces
  '(helm-selection ((t :background "gray25" :distant-foreground "black" :foreground "white smoke"))))

 ;; Helm buffer sort order is crazy without this; see
 ;; https://github.com/emacs-helm/helm/issues/1492
 (defun helm-buffers-sort-transformer@donot-sort (_ candidates _) candidates)
 (advice-add 'helm-buffers-sort-transformer :around 'helm-buffers-sort-transformer@donot-sort))

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-t") 'helm-projectile-find-file)
(setq helm-locate-command
      "glocate %s %s"
      helm-locate-create-db-command
      "gupdatedb --output='%s' --localpaths='%s'")

;; projectile
(setq projectile-project-search-path '("~/quip"))
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'native)

;; follow symlinks
(setq-default vs-follow-symlinks t)
(setq vc-follow-symlinks t)

;;;;;;;;;;;;;
;; bin/grr ;;
;;;;;;;;;;;;;

(require 'grep+)
(after! grep+
  (load-helper-file "grr.el"))

;;;;;;;;;;;;;;;;;;;
;; doom modeline ;;
;;;;;;;;;;;;;;;;;;;

;; Makes duplicate files show up as application.py|api instead of the <2>.
(require 'doom-modeline)
(after! doom-modeline
  (doom-modeline-mode t)
  (column-number-mode t)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-icon (display-graphic-p))
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-buffer-encoding nil))
;; if it doesn't appear, run M-x doom-modeline-mode (maybe should just run that
;; in here?)

;; display isearch counts (e.g. currently focused on search result i/n)
(global-anzu-mode +1)

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
;; , windows switch, and `next-error'
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice next-error (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(add-hook 'next-error-hook 'save-buffer)


;;;;;;;;;;;;;;
;; assorted ;;
;;;;;;;;;;;;;;

(after! pixel-scroll-precision-mode
  (pixel-scroll-precision-mode))
;;(after! ultra-scroll
;;  (setq scroll-conservatively 101 ; important!
;;        scroll-margin 0)
;;  (ultra-scroll-mode 1)
;;)

;; temp
;;(with-eval-after-load 'docker-tramp
;;  (unload-feature 'docker-tramp))

;; profile startup latency if enabled
;; https://github.com/doomemacs/doomemacs/issues/4498#issuecomment-753692913
(when init-file-debug
  (require 'benchmark-init)
  (add-hook 'doom-first-input-hook #'benchmark-init/deactivate))

;; splash screen
(setq fancy-splash-image (concat doom-private-dir "splash/doom-emacs-color.png"))

(global-set-key (kbd "M-o") 'new-scratch)

;; yapf and isort (don't auto-perform these upon save since it locks up emacs
;; for a sec)
(load-helper-file "isort.el")
(global-set-key (kbd "C-M-i") 'isort-current-buffer)
(after! anaconda-mode
  (define-key anaconda-mode-map (kbd "C-M-i") 'isort-current-buffer))

(load-helper-file "yapf.el")
(global-set-key (kbd "C-M-y") 'yapf-current-buffer)

(load-helper-file "rename-file-and-buffer.el")

;;;;;;;;;;;
;; modes ;;
;;;;;;;;;;;

;; line wrap
(global-visual-line-mode t)
(global-subword-mode)
(global-undo-tree-mode)

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

;; TODO: disabled due to high impact on startup performance
(require 'yasnippet)
(after! yasnippet
  (setq yas-snippet-dirs
        '("~/.config/doom/yasnippets"))
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-reload-all)
  (global-set-key (kbd "M-a") #'yas-next-field-or-maybe-expand)
  (define-key yas-minor-mode-map (kbd "M-a") yas-maybe-expand)

  (yas-global-mode 1))


;;;;;;;;;;;;;;
;; flycheck ;;
;;;;;;;;;;;;;;

;; (defun disable-flycheck-mode ()
;;   (interactive)
;;   (flycheck-mode -1)
;;   (global-flycheck-mode -1))
;; (add-hook 'prog-mode-hook 'disable-flycheck-mode)
;; (add-hook 'python-mode-hook 'disable-flycheck-mode)
;; (remove-hook 'prog-mode-hook 'flycheck-mode)
;; (remove-hook 'python-mode-hook 'flycheck-mode)

;; even if it is somehow enabled, all checkers will be disabled
;; (setq-default flycheck-disabled-checkers 'flycheck-checkers)

;;;;;;;;;;;;;
;; Haskell ;;
;;;;;;;;;;;;;

;; disabled due to high impact on startup performance
;; (require 'lsp)
;; (require 'lsp-haskell)
;; (after! (:and lsp lsp-haskell)
;;   ;; Hooks so haskell and literate haskell major modes trigger LSP setup
;;   (add-hook 'haskell-mode-hook #'lsp)
;;   (add-hook 'haskell-literate-mode-hook #'lsp))
;;
;; (load-helper-file "hs-lint.el")
;; (after! haskell-mode
;;   (define-key haskell-mode-map (kbd "C-c C-h") 'hs-lint))

;;;;;;;;;;;;;;;
;; cosmetics ;;
;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq doom-theme 'afternoon)
;;(setq doom-theme 'doom-acario-light)
(setq doom-font (font-spec :family "Source Code Pro" :size 18))

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

;; cursor
(setq-default cursor-type 'bar)
(after! hl-line
  (set-face-background hl-line-face "#333333")
  (set-face-attribute hl-line-face nil :underline t))

;; When M-q formatting a comment, only use one space instead of two
;; after a period.
(set-variable 'sentence-end-double-space nil)

;; font size
(load-helper-file "text-zoom.el")
(global-set-key (kbd "C-=") 'text-zoom/body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom (don't edit any of this) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t :height 3.5 :foreground "deep sky blue" :inherit (quote aw-leading-char-face))))
 '(company-preview-common ((t (:background "#21e824bc35b0"))))
 '(company-scrollbar-bg ((t (:background "#2bd12f784561"))))
 '(company-scrollbar-fg ((t (:background "#21e824bc35b0"))))
 '(company-tooltip ((t (:inherit default :background "#1bf61e4b2c46"))))
 '(company-tooltip-annotation ((t (:foreground "deep sky blue"))))
 '(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :foreground "deep sky blue" :weight bold))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(helm-selection ((t :background "gray25" :distant-foreground "black" :foreground "white smoke"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(recentf-auto-cleanup 60)
 '(package-selected-packages (quote (helm-swoop symbol-navigation-hydra))))
