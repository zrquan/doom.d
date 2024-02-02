;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! org-roam-ui :pin "9474a254390b1e42488a1801fed5826b32a8030b")

(package! org-modern
  :recipe (:host github :repo "minad/org-modern"))

;; https://github.com/org-roam/org-roam-bibtex#doom-emacs
(package! org-roam-bibtex :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(unpin! org-roam)

(package! dirvish)

(package! magit-stats :recipe (:host github :repo "LionyxML/magit-stats"))

;; (package! emacs-codeql :recipe (:host github :repo "anticomputer/emacs-codeql"))
;; (package! sicp :recipe (:host github :repo "webframp/sicp-info"))
;; (package! org-super-agenda :recipe (:host github :repo "alphapapa/org-super-agenda"))
