;;; ~/doom-config/ahs-hydra.el -*- lexical-binding: t; -*-

(defhydra hydra-auto-symbol-highlight (:hint nil)
  ;; this should be modeled after spacemacs//symbol-highlight-doc
  "
Current range: %(get-range-string)
^Navigation^       ^Search^          ^AHS Hydra^        ^Magic^
-----------------------------------------------------------------
_n_: next          ^ ^               _r_: range         _e_: iedit
_N_/_p_: previous    _f_: folder       _R_: reset         _s_: swoop
_d_: prevdef       _g_: project      _q_: cancel
_D_: nextdef
"
  ;; TODO: show i/n
  ("n" quick-ahs-forward)
  ("N" quick-ahs-backward)
  ("p" quick-ahs-backward)
  ("d" ahs-forward-definition)
  ("D" ahs-backward-definition)
  ;; TODO: show what the range is in the hydra description
  ("r" ahs-change-range)
  ("R" ahs-back-to-start)
  ("z" (progn (recenter-top-bottom) (ahs)))
  ("e" ahs-to-iedit :exit t)
  ("s" helm-swoop-region-or-symbol :exit t)
  ("f" (helm-projectile-ag-the-selection t) :exit t)
  ("g" (helm-projectile-ag-the-selection nil) :exit t)
  ("q" nil))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (spacemacs/defer-until-after-user-config                                    ;;
;;  '(lambda ()                                                                ;;
;;     (eval                                                                   ;;
;;      (append                                                                ;;
;;       '(defhydra ,func                                                      ;;
;;          (nil nil                                                           ;;
;;           :hint ,hint                                                       ;;
;;           :columns ,columns                                                 ;;
;;           :foreign-keys ,foreign-keys                                       ;;
;;           :body-pre ,entry-sexp                                             ;;
;;           :before-exit ,exit-sexp)                                          ;;
;;          ,doc)                                                              ;;
;;       (spacemacs//transient-state-adjust-bindings                           ;;
;;        ',bindings ',remove-bindings ',add-bindings)))                       ;;
;;     (when ,title                                                            ;;
;;       (let ((guide (concat "[" (propertize "KEY" 'face 'hydra-face-blue)    ;;
;;                            "] exits state  ["                               ;;
;;                            (if ',foreign-keys                               ;;
;;                                (propertize "KEY" 'face 'hydra-face-pink)    ;;
;;                              (propertize "KEY" 'face 'hydra-face-red))      ;;
;;                            "] will not exit")))                             ;;
;;         ;; (add-face-text-property 0 (length guide) '(:height 0.9) t guide) ;;
;;         (add-face-text-property 0 (length guide) 'italic t guide)           ;;
;;         (setq ,hint-var                                                     ;;
;;               (list 'concat                                                 ;;
;;                     (when dotspacemacs-show-transient-state-title           ;;
;;                       (concat                                               ;;
;;                        (propertize                                          ;;
;;                         ,title                                              ;;
;;                         'face 'spacemacs-transient-state-title-face)        ;;
;;                        (if ,hint-doc-p " " "\n"))) ,hint-var                ;;
;;                        ',dyn-hint                                           ;;
;;                        (when dotspacemacs-show-transient-state-color-guide  ;;
;;                          (concat "\n" guide))))))                           ;;
;;     ,@bindkeys)))))                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-range-string ()
  (let ((plighter (ahs-current-plugin-prop 'lighter)))
   (cond ((string= plighter "HS")  "Display")
                      ((string= plighter "HSA") "Buffer")
                      ((string= plighter "HSD") "Function"))
   )
  )


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
(global-set-key (kbd "M-t") 'ahs)

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
   (ahs-edit-mode t))

(defun helm-projectile-ag-the-selection (current-folder)
  "helm-projectile-ag the selection

  if current-folder is t, then searches the current folder. Otherwise, searches
  from the projectile directory root"
  (interactive)
  (projectile-helm-ag current-folder (thing-at-point 'symbol))
)

(defun projectile-helm-ag (arg query)
  "Run helm-do-ag relative to the project root.  Or, with prefix arg ARG, relative to the current directory."
  (interactive "P")
  (if arg
      (progn
        ;; Have to kill the prefix arg so it doesn't get forwarded
        ;; and screw up helm-do-ag
        (set-variable 'current-prefix-arg nil)

        (if dired-directory
            (helm-do-ag dired-directory nil query)
          (helm-do-ag (file-name-directory (buffer-file-name)) nil query)
          )
        )
    (helm-do-ag (projectile-project-root) nil query)
    ))
