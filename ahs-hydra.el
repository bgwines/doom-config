;;; ~/doom-config/ahs-hydra.el -*- lexical-binding: t; -*-

(defhydra hydra-auto-symbol-highlight (global-map "<f2>")
  ("n" quick-ahs-forward "next")
  ("N" ahs-backward "previous")
  ("p" quick-ahs-backward "previous")
;;  ("d" ahs-forward-definition "next definition")
;;  ("D" ahs-backward-definition "previous definition")
;;  ("r" ahs-change-range "change range")
;;  ("R" ahs-back-to-start "reset")
  ;; problem: loses highlights in some areas after a few iterations
  ("z" (progn (recenter-top-bottom) (ahs)) "recenter")
  ("e" ahs-to-iedit "iedit")
;;  ("s" swoop "swoop")
;;  ("b" buffers "buffers")
;;  ("b" buffers "buffers")
;;  ("f" files "files")
;;  ("/" project "project")
  ("q" nil :exit t))

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

;; problem: only cycles around within visible segment of buffer
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
