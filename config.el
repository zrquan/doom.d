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

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/"
      org-download-image-dir "~/Dropbox/org/org-download"
      org-agenda-files '("~/Dropbox/org/roam/daily/"))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; 窗口最大化
(push '(fullscreen . maximized) default-frame-alist)

(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; 用 df 代替 esc
(setq evil-escape-key-sequence "df")

(use-package! page-break-lines
  :commands page-break-lines-mode
  :init
  (autoload 'turn-on-page-break-lines-mode "page-break-lines")
  :config
  (setq page-break-lines-max-width fill-column)
  (map! :prefix "g"
        :desc "Prev page break" :nv "[" #'backward-page
        :desc "Next page break" :nv "]" #'forward-page))

;; org mode

(after! org
  (setq org-tags-column 0
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        indent-tabs-mode nil)
  (add-hook! 'org-mode-hook #'auto-fill-mode))

(after! org-superstar
  (setq org-superstar-headline-bullets-list '("¶" "#" 9673)
        org-superstar-cycle-headline-bullets nil))

;; org download
(after! org-download
  (setq org-download-method 'directory
        org-download-abbreviate-filename-function 'file-relative-name
        org-download-heading-lvl 0
        org-download-image-org-width 600))

(after! org-roam
  (setq +org-roam-open-buffer-on-find-file nil)
  (setq org-roam-title-sources '((title) alias))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t))))

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

(map! :map doom-leader-map "s y" 'youdao-dictionary-search-at-point-posframe)

;; Local Variables:
;; eval: (page-break-lines-mode 1)
;; End:
