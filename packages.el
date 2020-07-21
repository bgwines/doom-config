(package! helm
 :recipe (:host github
          :repo "emacs-helm/helm"
          :branch "f3cd86f171c4903a3a92812c3de36a95a0bd6fcd"))
(package! helm :pin "f3cd86f171c4903a3a92812c3de36a95a0bd6fcd")

(package! whole-line-or-region)  ;; what is this?
(package! afternoon-theme)
(package! helm-ag)
(package! helm-swoop)
(package! undo-tree)
(package! protobuf-mode)
(package! fastnav)
(package! git-link)
(package! backward-forward)
(package! hl-line)
(package! doom-modeline)
(package! package-lint)
(package! iedit)
(package! ace-window)
(package! move-text)
(package! anzu)
(package! hydra)
(package! symbol-navigation-hydra)
