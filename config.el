;;;;;;;;;;;;;;
;; SN Hydra ;;
;;;;;;;;;;;;;;

(require 'symbol-navigation-hydra)
(after! symbol-navigation-hydra
  (global-set-key (kbd "M-t") 'symbol-navigation-hydra-engage-hydra)
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

(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)
(after! git-link
  (setq-default git-link-use-commit t))

;;;;;;;;;;;;;
;; editing ;;
;;;;;;;;;;;;;

(load-file "~/doom-config/new-scratch.el")

;; undo
(global-set-key (kbd "M-_") 'undo-tree-redo)

;; line numbers
(load-file "goto-line.el")
(global-set-key (kbd "M-g M-g") 'hydra-goto-line/body)
(global-display-line-numbers-mode -1)
(setq display-line-numbers-type nil)

;; smartparens
(after! smartparens
  (load-file "~/doom-config/smartparens-hydra.el")
  (global-set-key (kbd "M-s") 'smartparens-hydra/body))

;; jump to start of line
(global-set-key (kbd "C-'") 'back-to-indentation)

;; move cursor to top and bottom of window, set to M-l for symmetry with C-l
(global-set-key (kbd "M-l") 'move-to-window-line-top-bottom)

;; something-to-char
(load-file "~/doom-config/mark-or-zap-to-chars.el")
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
(global-set-key (kbd "M-?") 'help-command)

;;;;;;;;;;;;;;
;; flycheck ;;
;;;;;;;;;;;;;;

(defun disable-flycheck-mode ()
  (interactive)
  (flycheck-mode -1))
(add-hook 'python-mode-hook 'disable-flycheck-mode)
(add-hook 'protobuf-mode-hook 'disable-flycheck-mode)  ;; hook doesn't exist?

(flycheck-mode -1)
(global-flycheck-mode nil)
(remove-hook 'prog-mode 'flycheck-mode)
(setq-default flycheck-disabled-checkers '(tsx-tide typescript-tide ada-gnat asciidoctor asciidoc awk-gawk bazel-buildifier c/c++-clang c/c++-gcc c/c++-cppcheck cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cuda-nvcc cwl d-dmd dockerfile-hadolint elixir-credo emacs-lisp emacs-lisp-checkdoc ember-template erlang-rebar3 erlang eruby-erubis eruby-ruumba fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-staticcheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json json-jq jsonnet less less-stylelint llvm-llc lua-luacheck lua markdown-markdownlint-cli markdown-mdl nix nix-linter opam perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc protobuf-prototool pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile python-mypy r-lintr racket rpm-rpmlint rst-sphinx rst ruby-rubocop ruby-standard ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar terraform terraform-tflint tex-chktex tex-lacheck texinfo textlint typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby yaml-yamllint javascript-tide jsx-tide))

;;;;;;;;;;;;;;
;; deletion ;;
;;;;;;;;;;;;;;

(load-file "~/doom-config/deletion-functions.el")

(defun set-deletion-bindings (mode-map)
  (define-key mode-map (kbd "C-i") 'delete-backward-char)
  (define-key mode-map (kbd "M-i") 'subword-backward-delete)
  (define-key mode-map (kbd "C-k") 'delete-line-no-kill))

(global-set-key (kbd "C-i") 'delete-backward-char)
(global-set-key (kbd "M-i") 'subword-backward-kill)
(global-set-key (kbd "M-k") 'kill-whole-line)

(set-deletion-bindings minibuffer-local-map)

(after! markdown-mode
  (set-deletion-bindings markdown-mode-map))

(after! helm-files
  (set-deletion-bindings helm-map)
  (set-deletion-bindings helm-read-file-map)
  (set-deletion-bindings helm-find-files-map))

(after! isearch
  (set-deletion-bindings isearch-mode-map))

;; moving a line
(load-file "move-lines.el")

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; spaces, not tabs
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "<tab>") 'indent-for-tab-command)

;; Always add newline at end of file.
(setq require-final-newline t)

;;;;;;;;;;;;;;;
;; cosmetics ;;
;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
  (load-file "~/doom-config/window-hydra.el")
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

(after! helm-mode
  (setq-default grep-find-ignored-directories '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "ext" "clients" "proto/int" "node_modules"))
  (setq-default grep-find-ignored-files '(".#*" "*.hi" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" ".gitignore" "**/*.csv" "**/*.pbxproj" "**/*.xcscheme" "**/package-lock.json" "**/yarn.lock" "**/function_catalog.pb" "**/*.svg" "*.pb.cc" "*.pb.h" "**/*.py.[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f].py" "android/.idea" "android/.settings" "android/assets" "android/automator" "android/gen" "android/google_play_services" "android/jni/syncer" "android/res/values-*/strings.xml" "apps/experimental/video-conference/src/vendor" "apps/experimental/old-image" "apps/einstein-discovery/app/@salesforce/charts" "aws_lambda/sfdc_security_upload/requests*" "clients/hubot" "clients/integrations/httplib2" "cocoa/Include" "core/testdata/saml" "data/element_translation_test.py" "desktop/mac/Desktop/*.lproj/Localizable.strings" "desktop/mac/Ext" "desktop/mac/Web" "desktop/win/Desktop/Resources/Strings.*.json" "desktop/win/Ext" "desktop/win/include" "desktop/win/packages" "desktop/win/Web" "**/ext/*" "htmlcov" "ios/build" "ios/Ext" "ios/Pods" "ios/Quip/*lproj/*" "ios/Quip/Images.xcassets" "ios/Web" "ios/Teams/Web" "playground" "proto/ext/*" "rust/prefixindex/*" "scripts/data" "settings/android-version-log" "settings/authorized_keys*" "settings/canned_docs" "settings/hostmap_autogen/*.known_hosts" "settings/ios-version-log" "settings/ios/push_certs" "settings/macosx-version-log" "settings/names" "settings/pdf" "settings/testrunner/last_run_speed.json" "settings/translations" "static/css/email/email.css" "static/css/ext" "static/css/salesforce-lightning-design-system-ltng.css" "static/css/salesforce-lightning-design-system-ltng.min.css" "static/fonts" "static/function_catalog.pbascii" "static/images" "static/js/elements/externs/elements_api_autogen.js" "static/js/ext" "static/js/marketing/marketing.js" "static/less/ext" "static/less/marketing/marketing.css" "static/testdata" "static/ts/tools/typings" "static/ts/tools/dist" "static/ts/tools/tsc-allow/dist/" "static/ts/tools/transform/ext" "syncer/ext" "templates/jinja2/docx" "templates/jinja2/xlsx/" "templates/marketing/svg-home-live-apps-hero.html/"))
  )

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
  (load-file "~/doom-config/grr.el"))

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

;;;;;;;;;;;;;;
;; assorted ;;
;;;;;;;;;;;;;;

;; yapf (don't auto-perform this upon save since it locks up emacs for a
;; sec)
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
