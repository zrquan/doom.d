;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu :pin "9c9be9c5709066adeaab1a4ddbe0956802351807")
(when (featurep! +orderless)
  (package! orderless :pin "87ab7e47e343285f7afd42779c78551332b8fd84"))
(when (featurep! +icon)
  (package! kind-all-the-icons
    :recipe (:local-repo "local")))
(package! cape :recipe (:host github :repo "minad/cape" :branch "main"))
