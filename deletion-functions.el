(defun delete-line-no-kill ()
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point))))

(defun delete-whole-line-no-kill ()
  (interactive)
  (delete-line-no-kill)
  (delete-char 1))

(defun subword-backward-delete ()
  (interactive)
  (subword-backward-kill 1)
  (setq kill-ring-yank-pointer (cdr kill-ring-yank-pointer)))
