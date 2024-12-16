;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-


;; Make `doom-variable-pitch-font' and `doom-font' have the same font, otherwise
;; there will be problems with Chinese scaling.
(setq! doom-font (font-spec :family "LXGW WenKai Mono" :size 30)
       doom-symbol-font (font-spec :family "LXGW WenKai Mono")
       doom-variable-pitch-font doom-font)

(setq default-frame-alist
      ;; require `dash.el'
      (-union default-frame-alist
              '((width . 120)
                (height . 43))))

(use-package! catppuccin-theme
  :config
  (setq! catppuccin-height-doc-title 1.3
         catppuccin-height-title-1 1.1
         catppuccin-height-title-2 1.0
         catppuccin-height-title-3 1.0))

(setq! doom-theme 'catppuccin
       doom-modeline-icon t
       fancy-splash-image "~/.doom.d/seele.png"
       display-line-numbers-type t
       initial-frame-alist '((top . 0.3) (left . 0.45) (width . 80) (height . 40)))

(after! vertico
  (vertico-indexed-mode 1)
  (setq! vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center))

(after! org
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-leading-stars t
   org-hide-emphasis-markers t)

  ;; Ellipsis styling
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil))

(after! org-modern
  (setq! org-modern-star 'replace
         org-modern-replace-stars "¶◈#"
         org-modern-priority t))
