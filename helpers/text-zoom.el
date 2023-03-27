(defun text-zoom-header ()
  (propertize "Text Zoom" 'face `(:box t :weight bold)))

(defhydra text-zoom (:hint nil)
  "
%s(text-zoom-header) _h_: +  _u_: - _0_/_z_: reset  _q_: quit
"
  ("h" (doom/increase-font-size 0.5))
  ("+" (doom/increase-font-size 0.5))
  ("u" (doom/decrease-font-size 0.5))
  ("-" (doom/decrease-font-size 0.5))
  ("0" doom/reset-font-size)
  ("z" doom/reset-font-size)
  ("q" nil :exit t))
