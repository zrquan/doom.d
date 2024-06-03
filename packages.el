;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! org-roam-ui)

(package! org-modern :recipe (:host github :repo "minad/org-modern"))

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

;; https://github.com/org-roam/org-roam-bibtex#doom-emacs
;; (package! org-roam-bibtex :recipe (:host github :repo "org-roam/org-roam-bibtex"))
;; (unpin! org-roam)

(package! dirvish)
