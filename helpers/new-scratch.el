(defun new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (defun format-name (num)
    (format "scratch-%03i" num))

  (defun get-new-scratch-buffer-name ()
    (interactive)
    (setq num 0)
    (while (get-buffer (format-name num))
      (setq num (+ 1 num)))
    (format-name num))

  (switch-to-buffer (get-new-scratch-buffer-name)))
