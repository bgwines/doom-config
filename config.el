
;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "<tab>") 'indent-for-tab-command)
(after! iedit-mode
  (define-key iedit-mode-keymap (kbd "<tab>") 'indent-for-tab-command))
(global-set-key (kbd "C-c C-l") 'eval-buffer)
(global-set-key (kbd "C-M-x") 'eval-defun)

;;;;;;;;;;;;;;;
;; SN Hydra ;;
;;;;;;;;;;;;;;;

(require 'symbol-navigation-hydra)
(after! symbol-navigation-hydra
  (global-set-key (kbd "M-t") 'symbol-navigation-hydra-engage-hydra)
  (setq-default ahs-case-fold-search nil)
  (setq-default ahs-default-range 'ahs-range-whole-buffer))
(setq-default ahs-idle-interval 999999999.0)
(setq-default ahs-inhibit-face-list (delete 'font-lock-doc-string-face ahs-inhibit-face-list))
(setq-default ahs-inhibit-face-list (delete 'font-lock-string-face ahs-inhibit-face-list))
(setq-default ahs-inhibit-face-list (delete 'font-lock-doc-face ahs-inhibit-face-list))

;;;;;;;;;;;;;
;; buffers ;;
;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; remapping lost keystrokes
(after! ibuffer
  (define-key ibuffer-mode-map (kbd "<tab>") 'ibuffer-forward-filter-group))

(when (fboundp 'windmove-default-keybindings) (windmove-default-keybindings))

(after! hydra
  (defun window-hydra-header ()
    (propertize "Window Hydra" 'face `(:box t :weight bold)))

  (defhydra window-hydra (:hint nil)
  "
%s(window-hydra-header)
^ ^   Split     | ^ ^   Switch^ ^    | ^ ^    Resize   ^ ^   | ^ ^Close
^-^-------------|-^-^---------^-^----|-^-^------^-^----------|-^-^-^-^------------
_|_: vertical   | _a_: any    _f_: → | _F_: →   _0_: balance | _y_: current
_-_: horizontal | _s_: swap   _b_: ← | _B_: ←   ^ ^          | _o_: other
^ ^             | ^ ^         _p_: ↑ | _P_: ↑   ^ ^          | _O_/_1_: all others
^ ^             | ^ ^         _n_: ↓ | _N_: ↓   ^ ^          | _q_: (quit)
"
    ("|" split-window-right :exit t)
    ("-" split-window-below :exit t)

    ("P" hydra-move-splitter-up)
    ("N" hydra-move-splitter-down)
    ("F" hydra-move-splitter-right)
    ("B" hydra-move-splitter-left)
    ("0" balance-windows)

    ("y" delete-window :exit t)
    ("o" ace-delete-window :exit t)
    ("O" delete-other-windows :exit t)
    ("1" delete-other-windows :exit t)

    ("p" windmove-up :exit t)
    ("n" windmove-down :exit t)
    ("f" windmove-right :exit t)
    ("b" windmove-left :exit t)
    ("a" ace-window :exit t)
    ("s" ace-swap-window :exit t)

    ("q" nil :exit t)))
(global-set-key (kbd "C-,") 'window-hydra/body)

;;;;;;;;;
;; git ;;
;;;;;;;;;

(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)
(after! git-link
  (setq-default git-link-use-commit t))

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

(defun goto-line-and-recenter ()
  (interactive)
  (call-interactively #'goto-line)
  (recenter-top-bottom))

(defhydra hydra-goto-line (goto-map ""
                           :pre (linum-mode 1)
                           :post (linum-mode -1))
  "goto-line"
  ("g" goto-line-and-recenter "go" :exit t)
  ("RET" goto-line-and-recenter "go" :exit t)
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))
(global-set-key (kbd "M-g M-g") 'hydra-goto-line/body)

(defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                                     :color pink
                                     :hint nil
                                     :post (deactivate-mark))
  "
  ^_p_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_b_   _f_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_n_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
  ("p" rectangle-previous-line)
  ("n" rectangle-next-line)
  ("b" rectangle-backward-char)
  ("f" rectangle-forward-char)
  ("d" kill-rectangle)                    ;; C-x r k
  ("y" yank-rectangle)                    ;; C-x r y
  ("w" copy-rectangle-as-kill)            ;; C-x r M-w
  ("o" open-rectangle)                    ;; C-x r o
  ("t" string-rectangle)                  ;; C-x r t
  ("c" clear-rectangle)                   ;; C-x r c
  ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
  ("N" rectangle-number-lines)            ;; C-x r N
  ("r" (if (region-active-p)
           (deactivate-mark)
         (rectangle-mark-mode 1)))
  ("u" undo nil)
  ("g" nil))

(defhydra hydra-smartparens (:hint nil)
  "
 Moving^^^^                       Slurp & Barf^^   Wrapping^^            Sexp juggling^^^^               Destructive
------------------------------------------------------------------------------------------------------------------------
 [_a_] beginning  [_n_] down      [_h_] bw slurp   [_R_]   rewrap        [_S_] split   [_t_] transpose   [_c_] change inner  [_w_] copy
 [_e_] end        [_N_] bw down   [_H_] bw barf    [_u_]   unwrap        [_s_] splice  [_A_] absorb      [_C_] change outer
 [_f_] forward    [_p_] up        [_l_] slurp      [_U_]   bw unwrap     [_r_] raise   [_E_] emit        [_k_] kill          [_g_] quit
 [_b_] backward   [_P_] bw up     [_L_] barf       [_(__{__[_] wrap (){}[]   [_j_] join    [_o_] convolute   [_K_] bw kill       [_q_] quit"
  ;; Moving
  ("a" sp-beginning-of-sexp)
  ("e" sp-end-of-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("n" sp-down-sexp)
  ("N" sp-backward-down-sexp)
  ("p" sp-up-sexp)
  ("P" sp-backward-up-sexp)

  ;; Slurping & barfing
  ("h" sp-backward-slurp-sexp)
  ("H" sp-backward-barf-sexp)
  ("l" sp-forward-slurp-sexp)
  ("L" sp-forward-barf-sexp)

  ;; Wrapping
  ("R" sp-rewrap-sexp)
  ("u" sp-unwrap-sexp)
  ("U" sp-backward-unwrap-sexp)
  ("(" sp-wrap-round)
  ("{" sp-wrap-curly)
  ("[" sp-wrap-square)

  ;; Sexp juggling
  ("S" sp-split-sexp)
  ("s" sp-splice-sexp)
  ("r" sp-raise-sexp)
  ("j" sp-join-sexp)
  ("t" sp-transpose-sexp)
  ("A" sp-absorb-sexp)
  ("E" sp-emit-sexp)
  ("o" sp-convolute-sexp)

  ;; Destructive editing
  ("c" sp-change-inner :exit t)
  ("C" sp-change-enclosing :exit t)
  ("k" sp-kill-sexp)
  ("K" sp-backward-kill-sexp)
  ("w" sp-copy-sexp)

  ("q" nil)
  ("g" nil))
(global-set-key (kbd "M-p") 'hydra-smartparens/body)


(add-hook 'prog-mode-hook #'backward-forward-mode)
(global-set-key (kbd "C-<left>") 'backward-forward-previous-location)
(global-set-key (kbd "C-<right>") 'backward-forward-next-location)
(after! smartparens
  (define-key smartparens-mode-map (kbd "C-<left>") 'backward-forward-previous-location)
  (define-key smartparens-mode-map (kbd "C-<right>") 'backward-forward-next-location))

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
(global-set-key (kbd "M-:") 'fastnav-mark-up-to-char-backward)
(global-set-key (kbd "M-Z") 'fastnav-mark-up-to-char-forward)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; comment out region/line
(global-set-key (kbd "C-/") 'whole-line-or-region-comment-dwim)

;; avy
(global-set-key (kbd "C-;") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-char-2)
;; remap default from C-; so we don't get the warning
(setq-default iedit-toggle-key-default nil)

;; multiple cursors
(global-set-key (kbd "M-u") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-h") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(setq mc/always-repeat-command t)
(setq mc/always-run-for-all t)

;; expand-region
(global-set-key (kbd "M-s") 'er/expand-region)

;; join-line
(global-set-key (kbd "C-j") 'join-line)

;;;;;;;;;;;;;;
;; flycheck ;;
;;;;;;;;;;;;;;

(defun disable-flycheck-mode ()
  (interactive)
  (flycheck-mode -1))
(add-hook 'python-mode-hook 'disable-flycheck-mode)
(add-hook 'elisp-mode-hook 'disable-flycheck-mode)

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

(after! markdown-mode
  (define-key markdown-mode-map (kbd "C-i") 'delete-backward-char))

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
(setq doom-font (font-spec :family "Source Code Pro" :size 22))


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
;; yapf takes care of this
;;(after! python
;;  (add-hook 'python-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow))))

;; font size
(defun text-zoom-header ()
  (propertize "Text Zoom" 'face `(:box t :weight bold)))

(defhydra text-zoom (:hint nil)
  "
%s(text-zoom-header) _h_: +  _u_: -  _0_: reset  _q_: quit
"
  ("h" doom/increase-font-size)
  ("+" doom/increase-font-size)
  ("u" doom/decrease-font-size)
  ("-" doom/decrease-font-size)
  ("0" doom/reset-font-size)
  ("q" nil :exit t))
(global-set-key (kbd "C-=") 'text-zoom/body)

;;;;;;;;;;;;;;;;
;; ace-window ;;
;;;;;;;;;;;;;;;;

(after! ace-window
  (global-set-key (kbd "M-o") 'ace-window)
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
  (ace-select-window)
  (find-file file))

(defun helm-buffer-ace-window (buffer)
  "Use ‘ace-window’ to select a window to display BUFFER."
  (ace-select-window)
  (helm-window-show-buffers (list buffer)))

(after! helm-buffers
 (add-to-list 'helm-type-buffer-actions
              '("Switch to buffer in Ace window ‘C-c C-e'" .
                helm-buffer-ace-window)
              :append)
 (define-key helm-buffer-map (kbd "C-c C-e") #'helm-buffer-run-ace-window)
 )

(after! helm-projectile
 (add-to-list 'helm-projectile-file-actions
              '("Switch to buffer in Ace window ‘C-c C-e'" .
                helm-buffer-ace-window)
              :append)
 (define-key helm-projectile-find-file-map (kbd "C-c C-e") #'helm-buffer-run-ace-window)
 )

(after! helm-files
  (add-to-list 'helm-find-files-actions
               '("Switch to file in Ace window ‘C-c C-e'" .
                 helm-file-ace-window)
               :append)
  (add-to-list 'helm-type-file-actions
               '("Switch to file in Ace window ‘C-c C-e'" .
                 helm-file-ace-window)
               :append)

  (define-key helm-map (kbd "C-c C-e") #'helm-file-run-ace-window)
  (define-key help-mode-map (kbd "C-c C-e") #'helm-file-run-ace-window)
  (define-key helm-find-files-map (kbd "C-c C-e") #'helm-file-run-ace-window)
  )

;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;

(after! (:and helm helm-buffers)
 (setq! helm-buffers-sort-fn #'helm-fuzzy-matching-sort-fn-preserve-ties-order)
 )

(defun helm-find-files-in-root ()
  "Open helm-find-files in the current project root."
  (interactive)
  (helm-find-files (doom-project-root))
  )
(global-set-key (kbd "C-c C-f") 'helm-find-files-in-root)

(after! helm
 ;; Default green selection color is hideous
 (custom-set-faces
  '(helm-selection ((t :background "gray25" :distant-foreground "black" :foreground "white smoke"))))

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

(after! helm-ag
  (defun proooojectile-helm-ag-dired-aware (arg)
    "Run `helm-do-ag' relative to the project root, searching for `QUERY'.

  Or, with prefix arg `ARG', search relative to the current directory."
    (interactive "P")
    (message "normal mode")
    (if arg
        (progn
          ;; Have to kill the prefix arg so it doesn't get forwarded
          ;; and screw up helm-do-ag
          (set-variable 'current-prefix-arg nil)
          (if dired-directory
              (helm-do-ag dired-directory)
            (helm-do-ag nil)
            )
          )
      (helm-do-ag nil)))

  (defun helm-projectile-ag-scoped (directory &optional options)
    (interactive "D")
    (if (projectile-project-p)
        (let* ((grep-find-ignored-files
                (cl-union (projectile-ignored-files-rel) grep-find-ignored-files))
               (grep-find-ignored-directories
                (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))
               (ignored (mapconcat (lambda (i)
                                     (concat "--ignore " i))
                                   (append grep-find-ignored-files
                                           grep-find-ignored-directories
                                           (cadr (projectile-parse-dirconfig-file)))
                                   " "))
               (helm-ag-base-command (concat helm-ag-base-command " " ignored " " options))
               (current-prefix-arg nil)
               (ag-directory (if directory directory (projectile-project-root))))
          (helm-do-ag ag-directory (car (projectile-parse-dirconfig-file)) nil))
      (error "You're not in a project"))
    )
  )

(global-set-key (kbd "M-g M-r") 'proooojectile-helm-ag-dired-aware)

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

;; follow symlinks
(setq-default vs-follow-symlinks t)
(setq vc-follow-symlinks t)

;;;;;;;;;;;;;;;;;;;
;; doom modeline ;;
;;;;;;;;;;;;;;;;;;;

;; Makes duplicate files show up as application.py|api instead of the <2>.
(setq doom-modeline-buffer-file-name-style 'relative-to-project)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-major-mode-color-icon t)
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-buffer-state-icon t)
(setq doom-modeline-buffer-modification-icon t)
(setq doom-modeline-minor-modes nil)
(setq doom-modeline-buffer-encoding nil)
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
(global-undo-tree-mode)

;; Disable line numbers
(global-display-line-numbers-mode -1)
(setq display-line-numbers-type nil)

;; line wrap
(global-visual-line-mode t)

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(after! yasnippet
  (setq yas-snippet-dirs
        '("~/.emacs.d/private/snippets"))
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-reload-all))

;;;;;;;;;;;;;
;; spotify ;;
;;;;;;;;;;;;;

(add-to-list 'load-path "/Users/bwines/doom-manually-cloned-packages/spotify.el")
;;(require 'spotify)
(after! spotify
  (setq spotify-oauth2-client-secret "d6af04f7574a4ec3b9c4b66b504fb806")
  (setq spotify-oauth2-client-id "8e2e1e959e54496eb6c7a623402f49b1")
  ;;(setq spotify-transport 'connect)
  (setq spotify-player-status-truncate-length 20) ; default: 15
  (setq spotify-player-status-playing-text "♬")
  (setq spotify-player-status-paused-text "‖")
  (setq spotify-player-status-format "[%p %a - %t]")
  (global-spotify-remote-mode))

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
 '(package-selected-packages (quote (helm-swoop symbol-navigation-hydra))))
