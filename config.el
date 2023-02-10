;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you.
(setq user-full-name "4shen0ne"
      user-mail-address "4shen.01@gmail.com")

;; Set the font to LXGW WenKai(霞鹜文楷).
;; Homepage: https://github.com/lxgw/LxgwWenKai
;;
;; Make `doom-variable-pitch-font' and `doom-font' have the same font, otherwise
;; there will be problems with Chinese scaling.
(setq doom-font (font-spec :family "LXGW WenKai Mono" :size 20)
      doom-unicode-font (font-spec :family "LXGW WenKai Mono")
      doom-variable-pitch-font doom-font)

;; Set path variables related to org-mode. I use dropbox to backup and sync my
;; org files, and integrate org-roam dailies with org-agenda-files.
(setq org-directory          "~/Dropbox/org/"
      org-id-locations-file  "~/Dropbox/org/.orgids"
      org-roam-directory     "~/Dropbox/org/roam/"
      org-agenda-files     '("~/Dropbox/org/roam/daily/")
      org-hugo-base-dir      "~/Dropbox/hugo/")

(setq doom-theme 'doom-nord-light)
(setq doom-modeline-icon nil)
(setq fancy-splash-image "~/.doom.d/banner.svg")
(setq display-line-numbers-type nil)

(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

;; Use chrome as default browser.
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome-stable")

(push '(fullscreen . maximized) default-frame-alist)

;; Use <df> instead of <Esc>.
(setq evil-escape-key-sequence "df")

(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ","
      doom-localleader-alt-key "M-,")

(setq url-gateway-method 'socks
      socks-server '("Default server" "127.0.0.1" 10808 5)
      url-gateway-local-host-regexp
      (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'"))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; 分割窗口时询问要打开的 buffer
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(after! org
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars t
        indent-tabs-mode nil
        org-capture-bookmark nil)
  (setq system-time-locale "C")         ;日期使用英文
  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")))
  (add-hook! 'org-mode-hook #'auto-fill-mode #'+org-init-keybinds-h #'global-org-modern-mode))

(after! org-modern
  (setq org-modern-star '("¶" "◈" "#")
        org-modern-priority nil
        org-modern-block-name '("⌜" . "⌞")))

;; (advice-remove 'org-download--delete #'+org--fix-org-download-delete-a)
(after! org-download
  (setq org-download-method 'directory
        ;; org-download-link-format "[[file:%s]]\n" ;保证顺利删除文件
        org-download-abbreviate-filename-function 'file-relative-name
        org-download-heading-lvl 0
        org-download-image-org-width 600))

;; (after! org-fancy-priorities
;;   (setq org-fancy-priorities-list '("" "" "")))

(after! org-roam
  (setq +org-roam-open-buffer-on-find-file nil)
  (setq org-roam-title-sources '((title) alias))
  (setq org-roam-capture-templates
        '(("n" " note" plain "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("j" " java" plain "%?"
           :if-new (file+head "java/${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t))

        org-roam-dailies-capture-templates
        '(("n" " note" entry "* %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
           :empty-lines-before 1
           :jump-to-captured t)
          ("t" " todo" entry "* TODO [#B] %?"
           :target (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n")
           :empty-lines-before 1)))
  ;; 调整 capture window 的高度
  (set-popup-rule! "^\\*Capture\\*$\\|CAPTURE-.*$" :size 0.4)

  (map! :leader
        :desc "Capture today" "n n" #'org-roam-dailies-capture-today
        :desc "Goto date" "n N" (lambda ()
                                  (interactive)
                                  (org-roam-dailies-goto-date nil "n"))))

;; Dependency of org-roam-ui
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(after! gist
  (setq gist-ask-for-filename t
        gist-ask-for-description t))

(after! company
  (setq company-format-margin-function #'company-text-icons-margin)
  ;; turnoff company-ispell
  (setq +company-backend-alist (assq-delete-all 'text-mode +company-backend-alist))
  (add-to-list '+company-backend-alist '(text-mode (:separate company-dabbrev company-yasnippet))))

;; 禁止 company 补全中文
(after! pyim
  (defun eh-company-dabbrev--prefix (orig-fun)
    (let ((string (pyim-char-before-to-string 0)))
      (if (pyim-string-match-p "\\cc" string)
          nil
        (funcall orig-fun))))
  (advice-add 'company-dabbrev--prefix :around #'eh-company-dabbrev--prefix)

  (setq pyim-indicator-cursor-color '("red")))

(after! pyvenv
  (map! :map python-mode-map
        :localleader
        :prefix ("v" . "pyvenv")
        :desc "workon"     "w" #'pyvenv-workon
        :desc "activate"   "a" #'pyvenv-activate
        :desc "deactivate" "d" #'pyvenv-deactivate
        :desc "create"     "c" #'pyvenv-create))

(map! :leader
      :desc "Translate word" "s w" #'youdao-dictionary-search-at-point-posframe
      :desc "Translate input" "s W" #'youdao-dictionary-search-from-input
      :desc "Kill buffer & window" "b x" #'kill-buffer-and-window)

(use-package! org-roam-bibtex
  :after org-roam
  :config
  (org-roam-bibtex-mode t))

(after! citar
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

        citar-symbols '((file "" . " ")
                        (note "" . " ")
                        (link "" . " "))

        citar-templates '((main . "${date year issued:4}     ${title:48}")
                          (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords keywords:*}")
                          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
                          (note . "Notes on ${author editor}, ${title}"))))

;; 在 terminal 使用系统剪贴板
(defadvice gui-backend-set-selection (around set-clip-from-terminal-on-osx activate)
  ad-do-it
  (when (and (equal system-type 'darwin)
             (not (display-graphic-p))
             (not (window-system))
             (equal (ad-get-arg 0) 'CLIPBOARD))
    (let ((process-connection-type nil)  ;; use pipe
          (default-directory "~/"))
      (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
        (process-send-string proc (ad-get-arg 1))
        (process-send-eof proc)))))

(use-package! dirvish
  :init (dirvish-override-dired-mode)
  :config
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (setq dirvish-cache-dir (concat doom-cache-dir "dirvish/")
        dirvish-attributes '(file-size all-the-icons vc-state)
        dirvish-quick-access-entries '(("h" "~/" "Home")
                                       ("d" "~/Downloads/" "Downloads")
                                       ("o" "~/Dropbox/org/" "Org")
                                       ("c" "~/code/" "Code")))
  (map! :map dired-mode-map
        :n "q" #'dirvish-quit
        :n "b" #'dirvish-quick-access
        :n "s" #'dirvish-quicksort
        :n "z" #'dirvish-history-jump
        :n "f" #'dirvish-file-info-menu
        :n "F" #'dirvish-layout-toggle
        :n "<tab>" #'dirvish-subtree-toggle
        :n "l" #'dired-find-file
        :n "h" #'dired-up-directory
        :n "C-h" #'dired-omit-mode))

(use-package! php-mode)

(map! :map emacs-lisp-mode-map
      :localleader
      :desc "edebug-remove-instrumentation" "d r" #'edebug-remove-instrumentation)

(setq lsp-warn-no-matched-clients nil)
