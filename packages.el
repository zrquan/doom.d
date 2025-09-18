;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; org-mode exts
(unpin! compat)                 ; https://github.com/minad/org-modern/issues/220
(package! org-modern)
(package! org-appear)
(package! org-super-agenda)
(package! org-download)
(package! org-tidy :recipe (:host github :repo "zrquan/org-tidy" :branch "tidy-attr"))
(package! verb)

;; my packages
(package! emacsploit :recipe (:local-repo "~/.doom.d/packages/emacsploit/"))
(package! gitmoji :recipe (:local-repo "~/.doom.d/packages/gitmoji/"))

(package! d2-mode)
(package! popper)
(package! ultra-scroll :recipe (:host github :repo "jdtsmith/ultra-scroll"))
;; (package! igist :recipe (:host github :repo "KarimAziev/igist"))

(package! prodigy)

;; 禁用一些我用不上的包
(disable-packages! centered-window org-tree-slide markdown-toc
                   pip-requirements anaconda-mode pipenv pyenv nose pyimport py-isort)

;; https://github.com/org-roam/org-roam-bibtex#doom-emacs
;; (package! org-roam-bibtex
;;   :recipe (:host github :repo "org-roam/org-roam-bibtex"))
;; (unpin! org-roam)
;; (unpin! bibtex-completion helm-bibtex ivy-bibtex)

(package! dirvish :pin "e8ec5765da1284be88b0cbf190362205a31fb19a")
