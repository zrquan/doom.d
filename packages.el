;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! org-roam-ui :pin "9474a254390b1e42488a1801fed5826b32a8030b")

(package! org-modern
  :recipe (:host github :repo "minad/org-modern"))

;; https://github.com/org-roam/org-roam-bibtex#doom-emacs
;; (package! org-roam-bibtex :recipe (:host github :repo "org-roam/org-roam-bibtex"))
;; (unpin! org-roam)

(package! dirvish)
