(defun isort-current-buffer ()
  (interactive)
  (shell-command-to-string
   (format "cd ~/quip && ~/quip/bin/isort %s" buffer-file-name)))
