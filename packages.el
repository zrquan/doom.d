;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


(package! org-roam-ui :pin "9474a254390b1e42488a1801fed5826b32a8030b")

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex")
  :pin "efdac6fe4134c33f50b06a0a6d192003d0e5094c")

(package! org-modern
  :recipe (:host github :repo "minad/org-modern"))

(package! dirvish)

(package! php-mode)

(package! sicp :recipe (:host github :repo "webframp/sicp-info"))
