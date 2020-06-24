;;; ~/doom-config/ahs-hydra.el -*- lexical-binding: t; -*-

(defhydra hydra-auto-symbol-highlight (global-map "<f2>")
  "The AHS Hydra"
  ("n" quick-ahs-forward "next")
  ("N" ahs-backward "previous")
  ("p" quick-ahs-backward "previous")
  ("d" ahs-forward-definition "next definition")
  ("D" ahs-backward-definition "previous definition")
  ;; the message doesn't stick; the hydra overrides it?
  ("r" ahs-change-range "change range")
  ("R" ahs-back-to-start "reset")
  ;; TODO: loses highlights in some areas after a few iterations
  ("z" (progn (recenter-top-bottom) (ahs)) "recenter")
  ("e" ahs-to-iedit "iedit")
  ("s" helm-swoop-region-or-symbol "swoop")
;;   ("b":    spacemacs/helm-buffers-smart-do-search-region-or-symbol "buffers")
;;   ("f":    spacemacs/helm-files-smart-do-search-region-or-symbol "files")
;;   ("/":    spacemacs/helm-project-smart-do-search-region-or-symbol "project")
  ("q" nil "cancel"))

;; taken from https://github.com/syl20bnr/spacemacs/pull/4332/files
(defun helm-swoop-region-or-symbol ()
  "Call `helm-swoop' with default input."
  (interactive)
  (let ((helm-swoop-pre-input-function
         (lambda ()
           (if (region-active-p)
               (buffer-substring-no-properties (region-beginning)
                                               (region-end))
             (let ((thing (thing-at-point 'symbol t)))
               (if thing thing ""))))))
    (call-interactively 'helm-swoop)))

(defun ahs ()
  "Highlight the symbol under point with `auto-highlight-symbol'."
  (interactive)
  (unless (bound-and-true-p ahs-mode-line)
    (auto-highlight-symbol-mode)
    )
  (ahs-highlight-now)
  (hydra-auto-symbol-highlight/body))
(global-set-key (kbd "C-c a h s") 'ahs)

(defun quick-ahs-forward ()
  "Go to the next occurrence of symbol under point with `auto-highlight-symbol'"
  (interactive)
  (quick-ahs-move t))

(defun quick-ahs-backward ()
  "Go to the previous occurrence of symbol under point with `auto-highlight-symbol'"
  (interactive)
  (quick-ahs-move nil))

;; TODO: default to WHOLE BUFFER (green range) (this is probably a global variable)
(defun quick-ahs-move (forward)
  "Go to the next occurrence of symbol under point with `auto-highlight-symbol'"
  (if forward
      (progn
        (ahs-highlight-now)
        (hydra-auto-symbol-highlight/body)
        (ahs-forward))
    (progn
      (ahs-highlight-now)
      (hydra-auto-symbol-highlight/body)
      (ahs-backward))))

(defun ahs-to-iedit ()
  "Trigger iedit from ahs."
  (interactive)
   (progn
    (iedit-mode)
    (iedit-restrict-region (ahs-current-plugin-prop 'start)
                           (ahs-current-plugin-prop 'end)))
   (t (ahs-edit-mode t)))























(defun aoeu ()
  (message "'sup"))
