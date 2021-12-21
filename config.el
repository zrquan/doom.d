;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "4shen0ne"
      user-mail-address "4shen.01@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string.
(setq doom-font (font-spec :family "Monaco" :size 14) ;14 15 16
      doom-unicode-font (font-spec :family "楷体-简" :size 16)) ;16 18 20

(defun +my/set-fonts ()
  (interactive)
  (set-fontset-font "fontset-default" 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
(add-hook! 'window-setup-hook :append '+my/set-fonts) ;🙂

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)
(setq doom-modeline-icon nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/"
      org-download-image-dir "~/Dropbox/org/org-download"
      org-agenda-files '("~/Dropbox/org/roam/daily/"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; 窗口最大化
(push '(fullscreen . maximized) default-frame-alist)

(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; 用 df 代替 esc
(setq evil-escape-key-sequence "df")

(after! org
  (setq org-hide-emphasis-markers t
        org-hide-leading-stars t
        indent-tabs-mode nil)
  (add-hook! 'org-mode-hook #'auto-fill-mode)
  (map! :g "C-," #'org-cycle-agenda-files
        :map org-mode-map
        "C-j" #'org-next-visible-heading
        "C-k" #'org-previous-visible-heading))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("¶" "#" 9673)
        org-superstar-cycle-headline-bullets nil))

(after! org-download
  (setq org-download-method 'directory
        org-download-link-format "[[file:%s]]\n" ;保证顺利删除文件
        org-download-abbreviate-filename-function 'file-relative-name
        org-download-heading-lvl 0
        org-download-image-org-width 600))

(after! org-fancy-priorities
  (setq org-fancy-priorities-list '("⚡" "🔖" "☕")))

(after! org-roam
  (setq +org-roam-open-buffer-on-find-file nil)
  (setq org-roam-title-sources '((title) alias))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)))
  (map! :leader
        :desc "Capture today" "n n" #'org-roam-dailies-capture-today
        :desc "Goto date" "n N" #'org-roam-dailies-goto-date))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; Mac 原生的全屏模式无法正常使用 posframe
(if IS-MAC
    (setq ns-use-native-fullscreen nil
          ns-use-fullscreen-animation nil))

(after! gist
  (setq gist-ask-for-filename t
        gist-ask-for-description t))

;; turnoff company-ispell
(after! company
  (setq +company-backend-alist (assq-delete-all 'text-mode +company-backend-alist))
  (add-to-list '+company-backend-alist '(text-mode (:separate company-dabbrev company-yasnippet))))

(map! :map doom-leader-map "s y" #'youdao-dictionary-search-at-point-posframe
      :map doom-leader-map "s w" #'youdao-dictionary-search-from-input)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; 分割窗口时询问要打开的 buffer
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

;; 取消中文补全
(after! pyim
  (defun eh-company-dabbrev--prefix (orig-fun)
    (let ((string (pyim-char-before-to-string 0)))
      (if (pyim-string-match-p "\\cc" string)
          nil
        (funcall orig-fun))))
  (advice-add 'company-dabbrev--prefix :around #'eh-company-dabbrev--prefix))

(after! pyvenv
  (map! :map python-mode-map
        :localleader
        :prefix ("v" . "pyvenv")
        :desc "workon"     "w" #'pyvenv-workon
        :desc "activate"   "a" #'pyvenv-activate
        :desc "deactivate" "d" #'pyvenv-deactivate
        :desc "create"     "c" #'pyvenv-create))
