;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; https://github.com/minad/org-modern/issues/220
(unpin! compat)
(package! org-modern :recipe (:host github :repo "minad/org-modern"))
(package! org-appear :recipe (:host github :repo "awth13/org-appear"))
(package! org-download :pin "900b7b6984d8fcfebbd3620152730228ce6468aa")
(package! org-tidy :recipe (:host github :repo "jxq0/org-tidy"))

;; 禁用一些我用不上的包
(disable-packages! centered-window org-tree-slide markdown-toc
                   pip-requirements anaconda-mode pipenv pyenv nose pyimport py-isort)

;; https://github.com/org-roam/org-roam-bibtex#doom-emacs
;; (package! org-roam-bibtex :recipe (:host github :repo "org-roam/org-roam-bibtex"))
;; (unpin! org-roam)

(package! dirvish)

;; (package! catppuccin-theme)

(package! verb)

(package! d2-mode)

;; (package! igist :recipe (:host github :repo "KarimAziev/igist"))

(package! emacsploit
  :recipe (:local-repo "~/.doom.d/packages/emacsploit/"))

(package! gitmoji
  :recipe (:local-repo "~/.doom.d/packages/gitmoji/"))
