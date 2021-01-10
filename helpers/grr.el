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

(define-key grep-mode-map (kbd "RET") 'grepp-open-result-in-ace-window)
(define-key grep-mode-map (kbd "M-RET") 'grepp-open-result-in-ace-window)

(defun grr-helper (grr-name query query-fn)
  (interactive)
  (unless (eq 1 (length (window-list)))
    (ace-select-window))

  ;; grr doesn't need this
  (setq-default grep-use-null-device nil)

  (defun display-same-window (buffer _)
    (display-buffer-same-window buffer nil))
  (setq-default display-buffer-overriding-action '(display-same-window . nil))
  (grep (format "~/quip/bin/%s %s" grr-name query))
  (setq-default display-buffer-overriding-action '(nil . nil))

  ;; it's already focused by default, so this isn't needed, I think
  ;;(let* ((pred (lambda (window)
  ;;               (let ((buf-name (buffer-name (window-buffer window))))
  ;;                 (message buf-name)
  ;;                 (string-prefix-p "*grep*" buf-name))))
  ;;       (window (get-window-with-predicate pred)))
  ;;  (select-window (window)))
  (grepp-rename-buffer-to-last-no-confirm)
  (read-only-mode 0)
  (delete-whole-line-no-kill)  ;; -*- mode: grep; default-directory: ...
  (delete-whole-line-no-kill)  ;; Grep started at Mon Dec 21 13:33:23
  (delete-whole-line-no-kill)  ;; empty line
  ;; (leave the line that contains the command that was run)

  ;; update buffer-local variables so that we can easily rerun the query
  (setq buffer-local-grr-query query)
  (setq buffer-local-grr-query-fn query-fn))

(defun grr (query)
  (interactive "sQuery: ")
  (grr-helper "absolute-grr" query 'grr))

(defun grr-server (query)
  (interactive "sQuery: ")
  (grr-helper "absolute-grr-server" query 'grr-server))

(defun grr-proto (query)
  (interactive "sQuery: ")
  (grr-helper "absolute-grr-proto" query 'grr-proto))

(defun grr-ekm (query)
  (interactive "sQuery: ")
  (grr-helper "absolute-grr-ekm" query 'grr-ekm))

(defun keep-lines-all (query)
  (interactive "sKeep lines containing match for regexp: ")
  (beginning-of-buffer)
  (keep-lines query))

(defun flush-lines-all (query)
  (interactive "sFlush lines containing match for regexp: ")
  (beginning-of-buffer)
  (flush-lines query))

(defun rerun-grr ()
  ;; TODO: same window
  (interactive)
  (funcall buffer-local-grr-query-fn buffer-local-grr-query))

(define-key grep-mode-map "k" 'keep-lines-all)
(define-key grep-mode-map "i" 'keep-lines-all)
(define-key grep-mode-map "f" 'flush-lines-all)
(define-key grep-mode-map "e" 'flush-lines-all)
(define-key grep-mode-map "r" 'rerun-grr)
(define-key grep-mode-map (kbd "C-k") 'delete-line-no-kill)
(define-key grep-mode-map (kbd "M-k") 'delete-whole-line-no-kill)

(setq buffer-local-grr-query-fn nil)
(setq buffer-local-grr-query nil)
(make-local-variable 'buffer-local-grr-query-fn)
(make-local-variable 'buffer-local-grr-query)

;; Surfaces the more useful commands (all of these are already in the
;; mode-map)
(defhydra grepp-hydra (:hint nil)
  (format "%s
^ ^ ^ ^  Filtering     | ^ ^ Buffers  ^^| ^ ^Other
^-^-^-^----------------|-^-^----------^^|-^-^-----
_k_/_i_: keep/include  | _+_/_n_: new   | _r_: rerun
_f_/_e_: flush/exclude | _g_: grep    ^^| _q_: quit
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
  ("r" rerun-grr :exit t)

  ("q" nil :exit t))
(define-key grep-mode-map "h" 'grepp-hydra/body) ;; [h]elp / [h]ydra

(global-set-key (kbd "M-g M-r M-r") 'grr)
(global-set-key (kbd "M-g M-r M-s") 'grr-server)
(global-set-key (kbd "M-g M-r M-p") 'grr-proto)
(global-set-key (kbd "M-g M-r M-e") 'grr-ekm)

(setq-default grep-highlight-matches t)
(setq-default grepp-default-comment-line-regexp ":[0-9]+: *#")