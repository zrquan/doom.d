;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-


(setq org-directory          "~/Dropbox/org/"
      org-id-locations-file  "~/Dropbox/org/.orgids"
      org-roam-directory     "~/Dropbox/org/roam/"
      org-agenda-files     '("~/Dropbox/org/roam/daily/")
      org-hugo-base-dir      "~/Dropbox/hugo/")

(after! org
  (add-hook! 'org-mode-hook #'auto-fill-mode #'+org-init-keybinds-h #'global-org-modern-mode)
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars t
        indent-tabs-mode nil
        org-capture-bookmark nil
        system-time-locale "C"          ;日期使用英文
        org-structure-template-alist '(("c" . "comment")
                                       ("e" . "example")
                                       ("q" . "quote")
                                       ("s" . "src")
                                       ("sb" . "src bash")
                                       ("sp" . "src python")
                                       ("sj" . "src java")
                                       ("sr" . "src restclient"))
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "IDEA(i)" "|" "DONE(d)" "KILL(k)"))))

(after! org-modern
  (setq org-modern-star '("¶" "◈" "#")
        org-modern-priority t
        org-modern-block-name '("⌜" . "⌞")))

(after! org-download
  (setq org-download-method 'directory
        org-download-link-format "[[file:%s]]\n" ;保证顺利删除文件
        org-download-abbreviate-filename-function 'file-relative-name
        org-download-heading-lvl 0
        org-download-image-org-width 800))

(after! org-roam
  ;; 调整 capture window 的高度
  ;; (set-popup-rule! "^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.4)
  (setq +org-roam-open-buffer-on-find-file nil
        org-roam-title-sources '((title) alias)

        org-roam-capture-templates
        '(("n" "󱞁 note" plain "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("j" " java" plain "%?"
           :if-new (file+head "java/${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t))

        org-roam-dailies-capture-templates
        '(("n" "󱞁 note" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
           :empty-lines-before 1
           :jump-to-captured t)
          ("t" " todo" entry "* TODO [#B] %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
           :empty-lines-before 1))))

(use-package! org-roam-ui
  :after org-roam
  :config
  (require 'websocket)
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package! org-roam-bibtex
  :after org-roam
  :config
  (org-roam-bibtex-mode t))

(after! citar
  (require 'citar-org-roam)
  (citar-register-notes-source
   'orb-citar-source (list :name "Org-Roam Notes"
                           :category 'org-roam-node
                           :items #'citar-org-roam--get-candidates
                           :hasitems #'citar-org-roam-has-notes
                           :open #'citar-org-roam-open-note
                           :create #'orb-citar-edit-note
                           :annotate #'citar-org-roam--annotate))

  (setq citar-notes-source 'orb-citar-source)
  (setq citar-file-open-functions (list (cons "html" #'citar-file-open-external)
                                        (cons "pdf" #'citar-file-open-external)
                                        (cons t #'find-file)))
  (setq citar-bibliography `(,(expand-file-name "ref.bib" org-directory))
        citar-library-paths `(,(expand-file-name "bibtex-pdfs" org-directory))
        citar-file-open-function (lambda (fpath)
                                   (if IS-MAC
                                       (call-process "open" nil 0 nil fpath)
                                     (browse-url-default-browser fpath)))
        citar-notes-paths `(,org-roam-directory)
        citar-open-note-function 'orb-citar-edit-note
        citar-templates '((main . "${date year issued:4}     ${title:48}")
                          (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
                          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                          (note . "Notes on ${author editor}, ${title}"))))
