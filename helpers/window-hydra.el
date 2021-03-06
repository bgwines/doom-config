(defun window-hydra-header ()
  (propertize "Window Hydra" 'face `(:box t :weight bold)))

(defvar window-hydra-should-exit t)

(defhydra window-hydra (:hint nil)
  "
%s(window-hydra-header)
^ ^   Split     | ^ ^   Switch^ ^    | ^ ^    Resize   ^ ^   | ^ ^Close
^-^-------------|-^-^---------^-^----|-^-^------^-^----------|-^-^-^-^------------
_|_: vertical   | _a_: any    _f_: → | _F_: →   _0_: balance | _y_: current
_-_: horizontal | _s_: swap   _b_: ← | _B_: ←   ^ ^          | _o_/_k_: other
^ ^             | ^ ^         _p_: ↑ | _P_: ↑   ^ ^          | _O_/_1_: all others
^ ^             | _m_: multi  _n_: ↓ | _N_: ↓   ^ ^          | _q_/_,_: (quit)
"
  ("|" split-window-right :exit window-hydra-should-exit)
  ("-" split-window-below :exit window-hydra-should-exit)

  ("P" hydra-move-splitter-up)
  ("N" hydra-move-splitter-down)
  ("F" hydra-move-splitter-right)
  ("B" hydra-move-splitter-left)
  ("0" balance-windows)

  ("y" delete-window :exit window-hydra-should-exit)
  ("o" ace-delete-window-wrapper :exit window-hydra-should-exit)
  ("k" ace-delete-window-wrapper :exit window-hydra-should-exit)
  ("O" delete-other-windows :exit window-hydra-should-exit)
  ("1" delete-other-windows :exit window-hydra-should-exit)

  ("p" windmove-up :exit window-hydra-should-exit)
  ("n" windmove-down :exit window-hydra-should-exit)
  ("f" windmove-right :exit window-hydra-should-exit)
  ("b" windmove-left :exit window-hydra-should-exit)
  ("a" ace-window :exit window-hydra-should-exit)
  ("s" ace-swap-window-wrapper :exit window-hydra-should-exit)

  ("m" window-hydra-toggle-should-exit)

  ("q" window-hydra-exit :exit t)
  ("," window-hydra-exit :exit t))

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

(defun window-hydra-toggle-should-exit ()
  (interactive)
  (setq window-hydra-should-exit (not window-hydra-should-exit)))

(defun window-hydra-exit ()
  "Clean up any state that needs to be cleaned up."
  (interactive)
  (setq window-hydra-should-exit t))
