(defun yapf-current-buffer ()
  (interactive)
  (shell-command-to-string
   (format "cd ~/quip && ~/quip/bin/yapf --inplace %s" buffer-file-name)))
