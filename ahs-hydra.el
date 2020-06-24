;;; ~/doom-config/ahs-hydra.el -*- lexical-binding: t; -*-

;; TODO search column
(defhydra hydra-auto-symbol-highlight (global-map "<f2>" :hint nil)
  "
^Navigation^       ^Search^            ^AHS Hydra^        ^Other^
^^^^^^^^-----------------------------------------------------------------
_n_: next          _s_: swoop          _r_: range         _e_: iedit
_N_/_p_: previous    _b_: buffer         _R_: reset
_d_: prevdef       _f_: files          _q_: cancel
_D_: nextdef       _/_: project
"
  ("n" quick-ahs-forward)
  ("N" quick-ahs-backward)
  ("p" quick-ahs-backward)
  ("d" ahs-forward-definition)
  ("D" ahs-backward-definition)
  ("r" ahs-change-range)
  ("R" ahs-back-to-start)
  ;; TODO: loses highlights in some areas after a few iterations
  ("z" (progn (recenter-top-bottom) (ahs)))
  ("e" ahs-to-iedit :color blue)  ;; TODO: use :exit t instead of color
  ("s" helm-swoop-region-or-symbol :color blue)
  ("b": nil :color blue) ;;spacemacs/helm-buffers-smart-do-search-region-or-symbol
  ("f": nil :color blue) ;;spacemacs/helm-files-smart-do-search-region-or-symbol
  ("/": nil :color blue) ;;spacemacs/helm-project-smart-do-search-region-or-symbol
  ("q" nil))

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
