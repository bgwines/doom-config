(defun window-hydra-header ()
  (propertize "Window Hydra" 'face `(:box t :weight bold)))

(defhydra window-hydra (:hint nil)
  "
%s(window-hydra-header)
^ ^   Split     | ^ ^   Switch^ ^    | ^ ^    Resize   ^ ^   | ^ ^Close
^-^-------------|-^-^---------^-^----|-^-^------^-^----------|-^-^-^-^------------
_|_: vertical   | _a_: any    _f_: → | _F_: →   _0_: balance | _y_: current
_-_: horizontal | _s_: swap   _b_: ← | _B_: ←   ^ ^          | _o_/_k_: other
^ ^             | ^ ^         _p_: ↑ | _P_: ↑   ^ ^          | _O_/_1_: all others
^ ^             | ^ ^         _n_: ↓ | _N_: ↓   ^ ^          | _q_: (quit)
"
  ("|" split-window-right :exit t)
  ("-" split-window-below :exit t)

  ("P" hydra-move-splitter-up)
  ("N" hydra-move-splitter-down)
  ("F" hydra-move-splitter-right)
  ("B" hydra-move-splitter-left)
  ("0" balance-windows)

  ("y" delete-window :exit t)
  ("o" ace-delete-window-wrapper :exit t)
  ("k" ace-delete-window-wrapper :exit t)
  ("O" delete-other-windows :exit t)
  ("1" delete-other-windows :exit t)

  ("p" windmove-up :exit t)
  ("n" windmove-down :exit t)
  ("f" windmove-right :exit t)
  ("b" windmove-left :exit t)
  ("a" ace-window :exit t)
  ("s" ace-swap-window-wrapper :exit t)

  ("q" nil :exit t))

(defun ace-swap-window-wrapper ()
  (interactive)
  (if (eq 2 (length (aw-window-list)))
      (aw-swap-window (cadr (window-list)))
    (ace-swap-window)))

(defun ace-delete-window-wrapper ()
  (interactive)
  (if (eq 2 (length (aw-window-list)))
      (aw-delete-window (cadr (window-list)))
    (ace-delete-window)))
