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

(defun grr-helper (grr-name query query-fn same-window repo)
  (interactive)
  ;;(unless (or (eq 1 (length (window-list))) same-window)
  ;;  (ace-select-window))

  ;; grr doesn't need this
  (setq-default grep-use-null-device nil)

  (defun display-same-window (buffer _)
    (display-buffer-same-window buffer nil))
  (setq-default display-buffer-overriding-action '(display-same-window . nil))
  (grep (format "~/%s/bin/%s %s" repo grr-name query))
  (setq-default display-buffer-overriding-action '(nil . nil))

  (grepp-rename-buffer-to-last-no-confirm)
  (message "setting read-only-mode 0 [START]")
  (read-only-mode 0)
  (message "setting read-only-mode 0 [DONE]")
  (beginning-of-buffer)

  ;; update buffer-local variables so that we can easily rerun the query
  (message "a")
  (setq buffer-local-grr-query query)
  (message "b")
  (setq buffer-local-grr-query-fn query-fn)
  (message "reached end"))

(defun mypy (query &optional same-window)
  (interactive "sQuery: ")
  (grr-helper "absolute-mypy" query 'mypy same-window "quip"))

(setq buffer-local-grr-query-fn nil)
(setq buffer-local-grr-query nil)
(make-local-variable 'buffer-local-grr-query-fn)
(make-local-variable 'buffer-local-grr-query)

(global-set-key (kbd "M-m M-y") 'mypy)

(setq-default grep-highlight-matches t)
(setq-default grepp-default-comment-line-regexp ":[0-9]+: *#")
