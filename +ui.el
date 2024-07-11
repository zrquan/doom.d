;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-


;; Make `doom-variable-pitch-font' and `doom-font' have the same font, otherwise
;; there will be problems with Chinese scaling.
(setq! doom-font (font-spec :family "LXGW WenKai Mono" :size 28)
       doom-symbol-font (font-spec :family "LXGW WenKai Mono")
       doom-variable-pitch-font doom-font)

(push '(alpha . 92) default-frame-alist)

(use-package! catppuccin-theme
  :config
  (custom-set-faces!
    '(diff-refine-added :inherit diff-added :background "#a6e3a1" :foreground "black")
    '(diff-refine-removed :inherit diff-removed :background "#f38ba8" :foreground "black"))
  (setq! catppuccin-height-doc-title 1.3
         catppuccin-height-title-1 1.1
         catppuccin-height-title-2 1.0
         catppuccin-height-title-3 1.0))

(setq! doom-theme 'catppuccin
       doom-modeline-icon t
       fancy-splash-image "~/.doom.d/seele.png"
       display-line-numbers-type t
       initial-frame-alist '((top . 0.3) (left . 0.45) (width . 80) (height . 40)))
