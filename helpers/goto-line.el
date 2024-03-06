(defun goto-line-and-recenter ()
  (interactive)
  (call-interactively #'goto-line)
  (recenter-top-bottom))

(defhydra hydra-goto-line (goto-map ""
                           :pre (global-display-line-numbers-mode 1)
                           :post (global-display-line-numbers-mode -1))
  "goto-line"
  ("g" goto-line-and-recenter "go" :exit t)
  ("RET" goto-line-and-recenter "go" :exit t)
  ("m" set-mark-command "mark" :bind nil)
  ("q" nil "quit"))
