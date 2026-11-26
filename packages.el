;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; org-mode exts
(package! org-modern)
(package! org-appear)
(package! org-download)
(package! org-tidy :recipe (:host github :repo "zrquan/org-tidy" :branch "tidy-attr"))
(package! verb)

;; my packages
(package! emacsploit :recipe (:local-repo "~/.doom.d/packages/emacsploit/"))
(package! gitmoji :recipe (:local-repo "~/.doom.d/packages/gitmoji/"))

(package! d2-mode)
(package! popper)

(package! prodigy)

;; 禁用一些我用不上的包
(disable-packages! centered-window org-tree-slide markdown-toc
                   pip-requirements pipenv pyenv nose)

(package! dirvish :pin "e8ec5765da1284be88b0cbf190362205a31fb19a")
