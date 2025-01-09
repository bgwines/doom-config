(defun grepp-open-result-in-curr-window2 ()
  (interactive)
  (if (eq 1 (length (window-list)))
      (progn
        (compile-goto-error)
        (recenter-top-bottom))
    (let ((display-buffer-overriding-action
           '(display-buffer-same-window (inhibit-same-window . nil))))
      (compile-goto-error))
    (recenter-top-bottom)))

(defun grepp-open-result-in-ace-window ()
  "Use ‘ace-window’ to select a window to display the grep+ result."
  (interactive)
  (let ((curr (current-buffer)))
    (if (eq 1 (length (window-list)))
        (progn
          (compile-goto-error)
          (recenter-top-bottom))
      (progn
        ;; `ace-select-window' uses `unwind-protect', so it's resilient to
        ;; `C-g'. Unfortunately, that makes it more challenging to detect
        ;; whether `C-g' ran at this level. It may be detectable via advising
        ;; something in there?
        (ace-select-window)
        (with-current-buffer curr
          (compile-goto-error))
        (recenter-top-bottom)))))

(define-key grep-mode-map (kbd "M-RET") 'grepp-open-result-in-curr-window2)

(defun grr-helper (executable-name query caller-fn same-window repo)
  (interactive)
  ;;(unless (or (eq 1 (length (window-list))) same-window)
  ;;  (ace-select-window))

  ;; grr doesn't need this
  (setq-default grep-use-null-device nil)

  (defun display-same-window (buffer _)
    (display-buffer-same-window buffer nil))
  (setq-default display-buffer-overriding-action '(display-same-window . nil))
  (grep (format "~/%s/bin/%s %s" repo executable-name query))
  (setq-default display-buffer-overriding-action '(nil . nil))

  (grepp-rename-buffer-to-last-no-confirm)
  (message "setting read-only-mode 0 [START]")
  (read-only-mode 0)
  (message "setting read-only-mode 0 [DONE]")
  (beginning-of-buffer)

  ;; update buffer-local variables so that we can easily rerun the query
  (setq buffer-local-prev-query query)
  (setq buffer-local-prev-caller-fn caller-fn))

(defun grr (query &optional same-window)
  (interactive "sQuery: ")
  (grr-helper "absolute-grr" query 'grr same-window "quip"))

(defun grr-server (query &optional same-window)
  (interactive "sQuery: ")
  (grr-helper "absolute-grr-server" query 'grr-server same-window "quip"))

(defun grr-proto (query &optional same-window)
  (interactive "sQuery: ")
  (grr-helper "absolute-grr-proto" query 'grr-proto same-window "quip"))

(defun grr-devops (query &optional same-window)
  (interactive "sQuery: ")
  (grr-helper "absolute-grr" query 'grr-devops same-window "devops"))

(defun mypy (&optional same-window)
  (interactive)
  (grr-helper "absolute-mypy" "--log-output-file=0" 'mypy same-window "quip"))

(defun keep-lines-all (query)
  (interactive "sKeep lines containing match for regexp: ")
  (beginning-of-buffer)
  (read-only-mode 0)
  (keep-lines query))

(defun flush-lines-all (query)
  (interactive "sFlush lines containing match for regexp: ")
  (beginning-of-buffer)
  (read-only-mode 0)
  (flush-lines query))

(defun rerun ()
  (interactive)
  (if (eq buffer-local-prev-caller-fn 'mypy)
      (funcall buffer-local-prev-caller-fn t)
      (funcall buffer-local-prev-caller-fn buffer-local-prev-query t)))

(define-key grep-mode-map "k" 'keep-lines-all)
(define-key grep-mode-map "i" 'keep-lines-all)
(define-key grep-mode-map "f" 'flush-lines-all)
(define-key grep-mode-map "e" 'flush-lines-all)
(define-key grep-mode-map "r" 'rerun)
(define-key grep-mode-map (kbd "C-k") 'delete-line-no-kill)
(define-key grep-mode-map (kbd "M-k") 'delete-whole-line-no-kill)

(setq buffer-local-prev-caller-fn nil)
(setq buffer-local-prev-query nil)
(make-local-variable 'buffer-local-prev-caller-fn)
(make-local-variable 'buffer-local-prev-query)

;; Surfaces the more useful commands (all of these are already in the
;; mode-map)
(defhydra grepp-hydra (:hint nil)
  (format "%s
^ ^ ^ ^  Filtering     | ^ ^ Buffers  ^^| ^ ^Other
^-^-^-^----------------|-^-^----------^^|-^-^-----
_k_/_i_: keep/include  | _+_/_n_: new   | _r_ ^ ^: rerun
_f_/_e_: flush/exclude | _g_: grep    ^^| _q_/_h_: quit
^ ^ ^ ^                | _b_: buffers ^^|
" (propertize "Grep+ Hydra" 'face `(:box t :weight bold)))
  ("k" keep-lines-all :exit t)
  ("i" keep-lines-all :exit t)
  ("f" flush-lines-all :exit t)
  ("e" flush-lines-all :exit t)

  ("+" grepp-new-grep-buffer :exit t)
  ("n" grepp-new-grep-buffer :exit t)
  ("g" grep :exit t)
  ("b" grepp-choose-grep-buffer :exit t)
  ("r" rerun :exit t)

  ("h" nil :exit t)
  ("q" nil :exit t))
(define-key grep-mode-map "h" 'grepp-hydra/body) ;; [h]elp / [h]ydra

(global-set-key (kbd "M-g M-r M-r") 'grr)
(global-set-key (kbd "M-g M-r M-s") 'grr-server)
(global-set-key (kbd "M-g M-r M-p") 'grr-proto)
(global-set-key (kbd "M-g M-r M-d") 'grr-devops)
(global-set-key (kbd "M-g M-y") 'mypy)

(setq-default grep-highlight-matches t)
(setq-default grepp-default-comment-line-regexp ":[0-9]+: *#")
