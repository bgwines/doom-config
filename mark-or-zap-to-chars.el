;; NOT sure why this fixes a thing but it definitely fixes something with the
;; next two commands
(defvaralias 'lazy-highlight-face 'isearch-lazy-highlight)

(defun mark-up-to-char-backward (arg char)
  (interactive "p\ncMark backwards up to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (set-mark (point))
  (goto-char (progn
                         (search-backward (char-to-string char)
                                         nil nil arg)
                         (forward-char)
                         (point))))

(defun zap-backwards-to-char (arg char)
  (interactive "p\ncZap backwards to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input char) char)))
  (kill-region (point) (progn
                         (search-backward (char-to-string char)
                                         nil nil arg)
                         (forward-char)
                         (point))))
