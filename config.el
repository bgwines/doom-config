
;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

(defun ahsrp ()
  (interactive)
  (message "%s" (ahs-runnable-plugins)))

(global-set-key (kbd "M-a") 'ahsrp)

(global-set-key (kbd "M-?") 'help-command)
(global-set-key (kbd "<tab>") 'indent-for-tab-command)
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

(after! hydra
  (defun window-hydra-header ()
    (propertize "Window Hydra" 'face `(:box t :weight bold)))

  (defhydra window-hydra (:hint nil)
  "
%s(window-hydra-header)
^ ^   Split     | ^ ^   Switch^ ^    | ^ ^    Resize   ^ ^   | ^ ^Close
^-^-------------|-^-^---------^-^----|-^-^------^-^----------|-^-^-^-^------------
_|_: vertical   | _a_: any    _f_: → | _F_: →   _0_: balance | _y_: current
_-_: horizontal | _s_: swap   _b_: ← | _B_: ←   ^ ^          | _o_/_k_: other
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
    ("o" ace-delete-window-wrapper :exit t)
    ("k" ace-delete-window-wrapper :exit t)
    ("O" delete-other-windows :exit t)
    ("1" delete-other-windows :exit t)

    ("p" windmove-up :exit t)
    ("n" windmove-down :exit t)
    ("f" windmove-right :exit t)
    ("b" windmove-left :exit t)
    ("a" ace-window :exit t)
    ("s" ace-swap-window-wrapper :exit t)

    ("q" nil :exit t)))

(defun ace-swap-window-wrapper ()
  (interactive)
  (if (eq 2 (length (aw-window-list)))
      (aw-swap-window (cadr (window-list)))
    (ace-swap-window)))

(defun ace-delete-window-wrapper ()
  (interactive)
  (if (eq 2 (length (aw-window-list)))
      (aw-delete-window (cadr (window-list)))
    (ace-delete-window)))

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
  (defun format-name (num)
    (format "scratch-%03i" num))

  (defun get-new-scratch-buffer-name ()
    (interactive)
    (setq num 0)
    (while (get-buffer (format-name num))
      (setq num (+ 1 num)))
    (format-name num))

  (switch-to-buffer (get-new-scratch-buffer-name)))

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
(global-display-line-numbers-mode -1)
(setq display-line-numbers-type nil)

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
  ("a" sp-beginning-of-sexp :exit t)
  ("e" sp-end-of-sexp :exit t)
  ("f" sp-forward-sexp :exit t)
  ("b" sp-backward-sexp :exit t)
  ("n" sp-down-sexp :exit t)
  ("N" sp-backward-down-sexp :exit t)
  ("p" sp-up-sexp :exit t)
  ("P" sp-backward-up-sexp :exit t)

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

;; jump to previous cursor locations
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

(defun mark-up-to-char-backward (arg char)
  (interactive "p\ncMark backwards up to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (set-mark (point))
  (goto-char (progn
                         (search-backward (char-to-string char)
                                         nil nil arg)
                         (forward-char)
                         (point))))

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
(global-set-key (kbd "M-:") 'mark-up-to-char-backward)
(global-set-key (kbd "M-Z") 'fastnav-mark-up-to-char-forward)
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
(add-hook 'python-mode-hook 'disable-flycheck-mode)
(add-hook 'protobuf-mode-hook 'disable-flycheck-mode)  ;; hook doesn't exist?

(flycheck-mode -1)
(global-flycheck-mode nil)
(remove-hook 'prog-mode 'flycheck-mode)
(setq-default flycheck-disabled-checkers '(tsx-tide typescript-tide ada-gnat asciidoctor asciidoc awk-gawk bazel-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint elixir-credo emacs-lisp emacs-lisp-checkdoc ember-template erlang-rebar3 erlang eruby-erubis eruby-ruumba fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json json-jq jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc protobuf-prototool pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-standard ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar terraform terraform-tflint tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby yaml-yamllint javascript-tide jsx-tide))

;;;;;;;;;;;;;;
;; deletion ;;
;;;;;;;;;;;;;;

;; backwards
(global-set-key (kbd "C-i") 'delete-backward-char)
(global-set-key (kbd "M-i") 'subword-backward-kill)
(global-set-key (kbd "M-I") 'backward-kill-sexp)

(define-key minibuffer-local-map (kbd "C-i") 'delete-backward-char)
(define-key minibuffer-local-map (kbd "M-i") 'subword-backward-delete)
(define-key minibuffer-local-map (kbd "C-k") 'delete-line-no-kill)

(defun subword-backward-delete ()
  (interactive)
  (subword-backward-kill 1)
  (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer))
  )

(after! markdown-mode
  (define-key markdown-mode-map (kbd "C-i") 'delete-backward-char))

(defun delete-line-no-kill ()
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point))))

(defun delete-whole-line-no-kill ()
  (interactive)
  (delete-line-no-kill)
  (delete-char 1))

(after! helm-files
  ;; neither of these seem to be having any effect
  (setq-default helm-ff-newfile-prompt-p t)
  (setq-default ffap-newfile-prompt t)
  (define-key helm-map (kbd "C-i") 'delete-backward-char)
  (define-key helm-map (kbd "M-i") 'subword-backward-delete)
  (define-key helm-map (kbd "C-k") 'delete-line-no-kill)
  (define-key helm-map (kbd "TAB") 'helm-ff-RET)
  (define-key helm-map (kbd "\t") 'helm-ff-RET)
  (define-key helm-read-file-map (kbd "C-i") 'delete-backward-char)
  (define-key helm-read-file-map (kbd "M-i") 'subword-backward-delete)
  (define-key helm-read-file-map (kbd "C-k") 'delete-line-no-kill)
  (define-key helm-read-file-map (kbd "TAB") 'helm-ff-RET)
  (define-key helm-read-file-map (kbd "\t") 'helm-ff-RET)
  (define-key helm-find-files-map (kbd "C-i") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "M-i") 'subword-backward-delete)
  (define-key helm-find-files-map (kbd "C-k") 'delete-line-no-kill)
  (define-key helm-find-files-map (kbd "TAB") 'helm-ff-RET)
  (define-key helm-find-files-map (kbd "\t") 'helm-ff-RET)
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
(setq doom-font (font-spec :family "Source Code Pro" :size 20))

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
  (defun projectile-helm-ag-dired-aware (arg)
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

(defun helm-ag-edit-run-ace-window ()
 (interactive)
 (with-helm-alive-p
   (helm-exit-and-execute-action 'helm-ag-edit-ace-window)))

(defun helm-ag-edit-ace-window (grep-line)
  "Use ‘ace-window’ to select a window to edit the grep results."
  (unless (eq 1 (length (window-list)))
    (ace-select-window))
  (helm-ag--edit grep-line))

(require 'helm-ag)
(after! helm-ag
  (define-key helm-ag-map (kbd "C-i") 'delete-backward-char)
  (define-key helm-ag-map (kbd "M-i") 'subword-backward-delete)
  (define-key helm-ag-map (kbd "C-k") 'delete-line-no-kill)
  (define-key helm-ag-map (kbd "C-c C-e") #'helm-ag-edit-run-ace-window)
  (define-key helm-ag-map (kbd helm-ace-command) #'helm-nonrelative-file-run-ace-window)
  (add-to-list 'helm-ag--actions
              '((format "Switch to file in Ace window ‘%s'" helm-ace-command) .
                helm-nonrelative-file-ace-window)
              :append)
  (add-to-list 'helm-ag--actions
              '((format "Edit results in Ace window ‘%s'" "C-c C-e") .
                helm-ag-edit-ace-window)
              :append)
  (setq-default helm-ag-use-grep-ignore-list t)
  )

(after! helm-mode
  (setq-default grep-find-ignored-directories '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "ext" "clients" "proto/int" "node_modules"))
  (setq-default grep-find-ignored-files '(".#*" "*.hi" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" ".gitignore" "**/*.csv" "**/*.pbxproj" "**/*.xcscheme" "**/package-lock.json" "**/yarn.lock" "**/function_catalog.pb" "**/*.svg" "*.pb.cc" "*.pb.h" "**/*.py.[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f].py" "android/.idea" "android/.settings" "android/assets" "android/automator" "android/gen" "android/google_play_services" "android/jni/syncer" "android/res/values-*/strings.xml" "apps/experimental/video-conference/src/vendor" "apps/experimental/old-image" "apps/einstein-discovery/app/@salesforce/charts" "aws_lambda/sfdc_security_upload/requests*" "clients/hubot" "clients/integrations/httplib2" "cocoa/Include" "core/testdata/saml" "data/element_translation_test.py" "desktop/mac/Desktop/*.lproj/Localizable.strings" "desktop/mac/Ext" "desktop/mac/Web" "desktop/win/Desktop/Resources/Strings.*.json" "desktop/win/Ext" "desktop/win/include" "desktop/win/packages" "desktop/win/Web" "**/ext/*" "htmlcov" "ios/build" "ios/Ext" "ios/Pods" "ios/Quip/*lproj/*" "ios/Quip/Images.xcassets" "ios/Web" "ios/Teams/Web" "playground" "proto/ext/*" "rust/prefixindex/*" "scripts/data" "settings/android-version-log" "settings/authorized_keys*" "settings/canned_docs" "settings/hostmap_autogen/*.known_hosts" "settings/ios-version-log" "settings/ios/push_certs" "settings/macosx-version-log" "settings/names" "settings/pdf" "settings/testrunner/last_run_speed.json" "settings/translations" "static/css/email/email.css" "static/css/ext" "static/css/salesforce-lightning-design-system-ltng.css" "static/css/salesforce-lightning-design-system-ltng.min.css" "static/fonts" "static/function_catalog.pbascii" "static/images" "static/js/elements/externs/elements_api_autogen.js" "static/js/ext" "static/js/marketing/marketing.js" "static/less/ext" "static/less/marketing/marketing.css" "static/testdata" "static/ts/tools/typings" "static/ts/tools/dist" "static/ts/tools/tsc-allow/dist/" "static/ts/tools/transform/ext" "syncer/ext" "templates/jinja2/docx" "templates/jinja2/xlsx/" "templates/marketing/svg-home-live-apps-hero.html/"))
  )

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

;;;;;;;;;;;;;
;; bin/grr ;;
;;;;;;;;;;;;;

(require 'grep+)
(after! grep+
  ;; TODO:
  ;;   * ace-window to choose in which window to display all results?
  (defun grepp-open-result-in-ace-window ()
    "Use ‘ace-window’ to select a window to display the grep+ result."
    (interactive)
    (let ((curr (current-buffer)))
      (unless (eq 1 (length (window-list)))
        (ace-select-window))
      (with-current-buffer curr
        (compile-goto-error)))
    (recenter-top-bottom))

  (define-key grep-mode-map (kbd "RET") 'grepp-open-result-in-ace-window)
  (define-key grep-mode-map (kbd "M-RET") 'grepp-open-result-in-ace-window)

  (defun grr-helper (grr-name query)
    (grep (format "~/quip/bin/%s %s" grr-name query))
    (select-window (get-buffer-window "*grep*"))
    (grepp-rename-buffer-to-last-no-confirm)
    (read-only-mode 0)
    (delete-whole-line-no-kill)  ;; -*- mode: grep; default-directory: ...
    (delete-whole-line-no-kill)  ;; Grep started at Mon Dec 21 13:33:23
    (delete-whole-line-no-kill)  ;; empty line
    ;; (leave the line that contains the command that was run)
    )

  (defun grr (query)
    (interactive "sQuery: ")
    (grr-helper "absolute-grr" query)
    )

  (defun grr-server (query)
    (interactive "sQuery: ")
    (grr-helper "absolute-grr-server" query)
    )

  (defun grr-proto (query)
    (interactive "sQuery: ")
    (grr-helper "absolute-grr-proto" query)
    )

  (defun grr-ekm (query)
    (interactive "sQuery: ")
    (grr-helper "absolute-grr-ekm" query)
    )

  (defun keep-lines-all (query)
    (interactive "sKeep lines containing match for regexp: ")
    (beginning-of-buffer)
    (keep-lines query)
    )

  (defun flush-lines-all (query)
    (interactive "sFlush lines containing match for regexp: ")
    (beginning-of-buffer)
    (flush-lines query)
    )

  (define-key grep-mode-map "k" 'keep-lines-all)
  (define-key grep-mode-map "i" 'keep-lines-all)
  (define-key grep-mode-map "f" 'flush-lines-all)
  (define-key grep-mode-map "e" 'flush-lines-all)
  (define-key grep-mode-map (kbd "C-k") 'delete-line-no-kill)
  (define-key grep-mode-map (kbd "M-k") 'delete-whole-line-no-kill)

  ;; Surfaces the more useful commands (all of these are already in the
  ;; mode-map)
  (defhydra grepp-hydra (:hint nil)
  (format "%s
^ ^ ^ ^  Filtering     | ^ ^ Buffers  |
^-^-^-^----------------|-^-^----------|
_k_/_i_: keep/include  | _+_/_n_: new   |
_f_/_e_: flush/exclude | _g_: grep    |
_q_:^ ^quit            | _b_: buffers |
" (propertize "Grep+ Hydra" 'face `(:box t :weight bold)))
    ("k" keep-lines-all :exit t)
    ("i" keep-lines-all :exit t)
    ("f" flush-lines-all :exit t)
    ("e" flush-lines-all :exit t)

    ("+" grepp-new-grep-buffer :exit t)
    ("n" grepp-new-grep-buffer :exit t)
    ("g" grep :exit t)
    ("b" grepp-choose-grep-buffer :exit t)

    ("q" nil :exit t))
  (define-key grep-mode-map "h" 'grepp-hydra/body) ;; [h]elp / [h]ydra

  (global-set-key (kbd "M-g M-r M-r") 'grr)
  (global-set-key (kbd "M-g M-r M-s") 'grr-server)
  (global-set-key (kbd "M-g M-r M-p") 'grr-proto)
  (global-set-key (kbd "M-g M-r M-e") 'grr-ekm)

  (setq-default grep-highlight-matches t)
  (setq-default grepp-default-comment-line-regexp ":[0-9]+: *#")
  )

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

;; rename-file (this already exists in some function; TODO: look that up)
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

;; line wrap
(global-visual-line-mode t)
(global-subword-mode)
(global-undo-tree-mode)

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
 '(recentf-auto-cleanup 60)
 '(package-selected-packages (quote (helm-swoop symbol-navigation-hydra))))
