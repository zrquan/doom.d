;;; $DOOMDIR/+org.el -*- lexical-binding: t; -*-


(after! org
  (map! :map org-mode-map
        :localleader
        :desc "org-emphasize" "X" #'org-emphasize
        :desc "org-download-delete" "a D" #'org-download-delete)

  ;; Fancy soft wrapping
  (setq dlukes/org-category-table (copy-category-table))
  (dolist (char '(?- ?+ ?_ ?| ?. ?, ?， ?； ?。))
    (modify-category-entry char ?| dlukes/org-category-table))
  (add-hook! '(org-mode-hook markdown-mode-hook)
    (set-category-table dlukes/org-category-table)
    (setq-local word-wrap-by-category t)
    (display-line-numbers-mode -1)
    (visual-line-fill-column-mode))
  (setq! visual-fill-column-width 90
         visual-fill-column-enable-sensible-window-split t
         visual-fill-column-center-text t)

  ;; https://emacs-china.org/t/org-mode/22313
  (font-lock-add-keywords 'org-mode
                          '(("\\cc\\( \\)[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)?\\cc?"
                             (1 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
                            ("\\cc?\\( \\)?[/+*_=~][^a-zA-Z0-9/+*_=~\n]+?[/+*_=~]\\( \\)\\cc"
                             (2 (prog1 () (compose-region (match-beginning 2) (match-end 2) "")))))
                          'append)
  (with-eval-after-load 'ox
    (defun eli-strip-ws-maybe (text _backend _info)
      (let* ((double-byte "[^\x00-\xff]")
             ;; remove whitespace from line break
             (text (replace-regexp-in-string
                    "\\(\\cc\\) *\n *\\(\\cc\\)"
                    "\\1\\2" text))
             ;; remove whitespace from `org-emphasis-alist'
             (text (replace-regexp-in-string (format "\\(\\cc\\) \\([`_\\*~]%s.*?%s[`_\\*~]\\) \\(\\cc\\)" double-byte double-byte)
                                             "\\1\\2\\3" text))
             ;; restore whitespace between English words and Chinese words
             (text (replace-regexp-in-string "\\(\\cc\\)\\(\\(?:<[^>]+>\\)?[a-z0-9A-Z-]+\\(?:<[^>]+>\\)?\\)\\(\\cc\\)"
                                             "\\1 \\2 \\3" text)))
        text))
    (add-to-list 'org-export-filter-paragraph-functions #'eli-strip-ws-maybe))

  (evil-define-key '(normal visual) evil-org-mode-map
    (kbd "C-j") #'org-forward-element
    (kbd "C-k") #'org-backward-element
    (kbd "z z") #'visual-line-fill-column-mode
    (kbd "z t") #'org-tidy-toggle)
  (evil-define-key '(insert) evil-org-mode-map
    (kbd "C-n") #'evil-next-line
    (kbd "C-p") #'evil-previous-line)
  ;; 让 `evil-org-mode' 在打开 org 文件前生效
  (evil-org-mode 1)

  (global-org-modern-mode 1)
  (org-download-enable)
  (add-hook! 'org-mode-hook
             #'+org-init-keybinds-h
             #'org-appear-mode)
  (dolist (template '(("ss" . "src shell")
                      ("sj" . "src json")
                      ("sd" . "src d2")
                      ("se" . "src elisp")
                      ("sp" . "src python")
                      ("sJ" . "src java")))
    (add-to-list 'org-structure-template-alist template))
  (setq! indent-tabs-mode nil
         org-capture-bookmark nil
         system-time-locale "C"          ;日期使用英文
         org-footnote-section "References"
         org-todo-keywords
         '((sequence "TODO(t)" "READ(r)" "FIXME(f)" "WAIT(w@)" "IDEA(i)" "|" "DONE(d!)" "KILL(k)")))

  (setq
   ;; Capture templates
   org-capture-templates
   '(("t" "Todo" entry
      (file+headline +org-capture-todo-file "Inbox")
      "** TODO %?\n" :prepend t)
     ("e" "Event" entry (file+headline +org-capture-todo-file "Calendar")
      "* TODO %?\n SCHEDULED: %^{Event Date}t"))

   ;; Edit settings
   ;; org-auto-align-tags nil
   org-tags-column -77
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-leading-stars t
   org-hide-emphasis-markers t
   org-startup-with-inline-images t
   org-image-align 'center

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-start-day "-1d"
   org-agenda-time-grid
   '((daily today weekly require-timed remove-match)
     (0 900 1130 1400 1600 1800 2100 2400)
     "……"
     "──────────────────────")
   org-agenda-current-time-string
   "← 现在 ──────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis "𐓷")
  (add-hook! '(org-mode-hook)
    (set-face-attribute 'org-document-title nil :height 1.3)
    (set-face-attribute 'org-level-1 nil :height 1.1)))

(after! org-modern
  (setq! org-modern-star 'replace
         org-modern-replace-stars "󰚟󰽺＃◈"
         org-modern-priority nil
         org-modern-keyword
         (quote (("title" . "")
                 ("filetags" . "")
                 ("attr_org" . "🄾")
                 ("attr_html" . "🄷")
                 ("caption" . "≡")
                 ("startup" . "")
                 ("html" . "")
                 (t . t)))
         org-modern-list '((?+ . "⇝")
                           (?- . "︎▪"))
         org-modern-checkbox '((?X . "✓︎")
                               (?- . "✗︎")
                               (?\s . "☐"))
         org-modern-block-name
         '((t . t)
           ("src" "»" "«")
           ("example" "⁗" "⁗")
           ("quote" "❝" "❞")
           ("export" "⇛" "⇚")
           ("comment" "##" "##"))
         org-modern-todo-faces
         (quote (("READ" :background "SkyBlue4" :foreground "white")
                 (t . nil)))))

(use-package! d2-mode
  :config
  (setq d2-flags '("-s" "-t" "4" "--dark-theme" "200")))

(after! org-download
  (setq! org-download-method 'directory
         org-download-link-format "[[file:%s]]\n"
         org-download-abbreviate-filename-function 'file-relative-name
         org-download-heading-lvl nil
         org-download-image-attr-list '("#+attr_org: :width 90%")
         org-download-display-inline-images nil))

(after! org-roam
  ;; 调整 capture window 的高度
  (set-popup-rule! "^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.7)
  (setq +org-capture-frame-parameters
        '((name . "doom-capture")
          ;; (top . ,(car (cdr (mouse-position))))
          ;; (left . ,(cdr (cdr (mouse-position))))
          (width . 70)
          (height . 25)
          (transient . t)
          (window-system . x)
          (display . ":1")
          nil))
  (setq! +org-roam-open-buffer-on-find-file nil
         org-roam-title-sources '((title) alias)
         org-roam-buffer-postrender-functions '(magit-section-show-level-2)
         +org-capture-fn #'org-roam-dailies-capture-today

         org-roam-capture-templates
         '(("d" "󱞁 default" plain "%?"
            :if-new (file+head "${slug}.org" "#+title: ${title}\n")
            :empty-lines-before 1
            :unnarrowed t)
           ("c" "󱙓 cheatsheet" plain "%?"
            :if-new (file+head "cheatsheet/${slug}.org" "#+title: ${title}\n")
            :empty-lines-before 1
            :unnarrowed t))

         org-roam-dailies-capture-templates
         '(("d" "󱞁 default" entry "* %?"
            :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
            :empty-lines-before 1
            :jump-to-captured nil))))

(use-package! org-tidy
  :hook (org-mode)
  :config
  (setq org-tidy-attr-flag t
        org-tidy-general-drawer-flag nil
        org-tidy-properties-style 'fringe))

(use-package! verb
  :init
  (map! :map org-mode-map
        :localleader
        :prefix ("v" . "verb")
        :desc "send request stay" "v" #'verb-send-request-on-point-other-window-stay
        :desc "send request" "V" #'verb-send-request-on-point-other-window
        :desc "send request no window" "s" #'verb-send-request-on-point-no-window
        :desc "show vars" "x" #'verb-show-vars
        :desc "kill response buffers" "k" #'verb-kill-all-response-buffers)
  :config (progn
            (setq verb-trim-body-end "[ \t\n\r]+")))

;; (use-package! org-roam-bibtex
;;   :after org-roam
;;   :config
;;   (org-roam-bibtex-mode t))

(after! citar
  (require 'citar-org-roam)
  ;; (citar-register-notes-source
  ;;  'orb-citar-source (list :name "Org-Roam Notes"
  ;;                          :category 'org-roam-node
  ;;                          :items #'citar-org-roam--get-candidates
  ;;                          :hasitems #'citar-org-roam-has-notes
  ;;                          :open #'citar-org-roam-open-note
  ;;                          :create #'orb-citar-edit-note
  ;;                          :annotate #'citar-org-roam--annotate))

  ;; Icon
  ;; (defvar citar-indicator-files-icon
  ;;   (citar-indicator-create
  ;;    :symbol (nerd-icons-faicon
  ;;             "nf-fa-file_pdf_o"
  ;;             :face 'nerd-icons-lred)
  ;;    :function #'citar-has-files
  ;;    :emptysymbol "  "
  ;;    :padding " "
  ;;    :tag "has:files"))
  ;; (defvar citar-indicator-notes-icon
  ;;   (citar-indicator-create
  ;;    :symbol (nerd-icons-sucicon
  ;;             "nf-custom-orgmode"
  ;;             :face 'nerd-icons-lgreen)
  ;;    :function #'citar-has-notes
  ;;    :emptysymbol "  "
  ;;    :padding " "
  ;;    :tag "has:notes"))
  ;; (defvar citar-indicator-links-icon
  ;;   (citar-indicator-create
  ;;    :symbol (nerd-icons-octicon
  ;;             "nf-oct-link"
  ;;             :face 'nerd-icons-lblue)
  ;;    :function #'citar-has-links
  ;;    :emptysymbol "  "
  ;;    :padding " "
  ;;    :tag "has:links"))
  ;; (defvar citar-indicator-cited-icon
  ;;   (citar-indicator-create
  ;;    :symbol (nerd-icons-octicon
  ;;             "nf-oct-book"
  ;;             :face 'nerd-icons-lred)
  ;;    :function #'citar-is-cited
  ;;    :emptysymbol "  "
  ;;    :padding " "
  ;;    :tag "is:cited"))
  ;; (setq citar-indicators
  ;;       (list citar-indicator-files-icon
  ;;             citar-indicator-notes-icon
  ;;             citar-indicator-links-icon
  ;;             citar-indicator-cited-icon))

  ;; (setq! citar-notes-source 'orb-citar-source)
  (setq! citar-file-open-functions (list (cons "html" #'citar-file-open-external)
                                         (cons "pdf" #'citar-file-open-external)
                                         (cons t #'find-file)))
  (setq! citar-bibliography `(,(expand-file-name "ref.bib" org-directory))
         citar-library-paths `(,(expand-file-name "bibtex-pdfs" org-directory))
         citar-file-open-function (lambda (fpath)
                                    (if IS-MAC
                                        (call-process "open" nil 0 nil fpath)
                                      (browse-url-default-browser fpath)))
         citar-notes-paths `(,org-roam-directory)
         ;; citar-open-note-function 'orb-citar-edit-note
         citar-templates '((main . "${date year issued:4}     ${title:48}")
                           (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
                           (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                           (note . "Notes on ${author editor}, ${title}"))))

(after! ox-hugo
  ;; (advice-add 'org-hugo--todo :around
  ;;             (lambda (_fun todo _info)
  ;;               (format "[%s]" todo)))
  (add-to-list 'org-hugo-tag-processing-functions
               (lambda (tag-list info)
                 (remove org-attach-auto-tag tag-list)))
  ;; functions
  (defun ox-hugo/export-all (&optional org-files-root-dir do-recurse)
    "Export all Org files under ORG-FILES-ROOT-DIR.

All valid post subtrees in all Org files are exported using
`org-hugo-export-wim-to-md'.

If optional arg ORG-FILES-ROOT-DIR is nil, all Org files in
current buffer's directory are exported.

If optional arg DO-RECURSE is non-nil, all Org files in
ORG-FILES-ROOT-DIR in all subdirectories are exported. Else, only
the Org files directly present in the current directory are
exported. If this function is called interactively with
\\[universal-argument] prefix, DO-RECURSE is set to non-nil.

Example usage in Emacs Lisp: (ox-hugo/export-all \"~/org\")."
    (interactive)
    (let* ((org-files-root-dir (or org-files-root-dir default-directory))
           (do-recurse (or do-recurse (and current-prefix-arg t)))
           (search-path (file-name-as-directory (expand-file-name org-files-root-dir)))
           (org-files (if do-recurse
                          (directory-files-recursively search-path "\.org$")
                        (directory-files search-path :full "\.org$")))
           (num-files (length org-files))
           (cnt 1))
      (if (= 0 num-files)
          (message (format "No Org files found in %s" search-path))
        (progn
          (message (format (if do-recurse
                               "[ox-hugo/export-all] Exporting %d files recursively from %S .."
                             "[ox-hugo/export-all] Exporting %d files from %S ..")
                           num-files search-path))
          (dolist (org-file org-files)
            (with-current-buffer (find-file-noselect org-file)
              (message (format "[ox-hugo/export-all file %d/%d] Exporting %s" cnt num-files org-file))
              (org-hugo-export-wim-to-md :all-subtrees)
              (setq cnt (1+ cnt))))
          (message "Done!")))))

  (defun zrquan/org-hugo-export-and-fix-relref ()
    (interactive)
    (let* ((md-file (org-hugo-export-to-md))
           (command (format "sed -i 's/relref \"..\\//relref \"braindump\\//g' %s" md-file)))
      (if (zerop (shell-command command))
          (message (format "Fixed relref links in %s" md-file))
        (error "Failed to execute sed"))))

  (defun zrquan/org-hugo-export-roam-refs (_)
    "导出 ROAM_REFS 引用的链接"
    (let ((refs (org-entry-get (point) "ROAM_REFS")))
      (when refs
        (save-excursion
          (goto-char (point-max))
          (insert "* Refs\n")
          (dolist (ref (split-string refs))
            (insert (format "- %s\n" ref)))))))
  (add-to-list 'org-export-before-parsing-functions #'zrquan/org-hugo-export-roam-refs))

(after! org-re-reveal
  (setq! org-re-reveal-extra-css (file-name-concat doom-user-dir "resources/reveal.css")
         ;; org-re-reveal-extra-scripts `(,(file-name-concat doom-user-dir "resources/reveal.js"))
         org-re-reveal-transition "slide"
         org-re-reveal-highlight-css 'monokai
         org-re-reveal-plugins '(zoom notes highlight)))

(use-package! org-super-agenda
  :init (add-hook 'org-agenda-mode-hook #'org-super-agenda-mode)
  :config
  (setq! org-super-agenda-header-map nil
         org-super-agenda-groups
         '((:name "今日任务"
            :scheduled today
            :deadline today
            :and (:scheduled past :not (:deadline past)))
           (:name "优先完成"
            :and (:priority "A"
                  :not (:todo "WAIT")))
           (:name "工作" :tag "@work" :tag "work" :tag "meeting")
           (:name "日程" :scheduled t :deadline t)
           (:order-multi (3 (:file-path "daily")
                            (:priority<= "B")
                            (:todo ("IDEA" "READ"))))
           (:todo "WAIT" :order 9))))
