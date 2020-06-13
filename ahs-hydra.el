;;; ~/doom-config/ahs-hydra.el -*- lexical-binding: t; -*-

;; (defhydra hydra-auto-symbol-highlight (global-map "<f2>")
;;   ("d" ahs-forward-definition "next definition")
;;   ("D" ahs-backward-definition "previous definition")
;;   ("e" ahs-to-iedit "iedit")
;;   ("n" ahs-forward "next")
;;   ("N" ahs-backward "previous")
;;   ("p" ahs-backward "previous")
;;   ("R" ahs-back-to-start "reset")
;;   ("r" ahs-change-range "change range")
;;   ("q" nil :exit t))

;; (defun ahs ()
;;   "Highlight the symbol under point with `auto-highlight-symbol'."
;;   (interactive)
;;   (unless (bound-and-true-p ahs-mode-line)
;;     (auto-highlight-symbol-mode)
;;     )
;;   (ahs-highlight-now)
;;   (hydra-auto-symbol-highlight/body))

;; (defun quick-ahs-forward ()
;;   "Go to the next occurrence of symbol under point with `auto-highlight-symbol'"
;;   (interactive)
;;   (quick-ahs-move t))

;; (defun quick-ahs-backward ()
;;   "Go to the previous occurrence of symbol under point with `auto-highlight-symbol'"
;;   (interactive)
;;   (quick-ahs-move nil))

;; (defun quick-ahs-move (forward)
;;   "Go to the next occurrence of symbol under point with `auto-highlight-symbol'"
;;   (if forward
;;       (progn
;;         (hydra-auto-symbol-highlight/body)
;;         (ahs-forward))
;;     (progn
;;       (hydra-auto-symbol-highlight/body)
;;       (ahs-backward))))
