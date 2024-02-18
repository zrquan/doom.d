;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-


;; Make `doom-variable-pitch-font' and `doom-font' have the same font, otherwise
;; there will be problems with Chinese scaling.
(setq doom-font (font-spec :family "LXGW WenKai Mono" :size 28)
      doom-symbol-font (font-spec :family "LXGW WenKai Mono")
      doom-variable-pitch-font doom-font)

(push '(alpha . 92) default-frame-alist)

(setq doom-theme 'doom-dracula
      doom-modeline-icon t
      fancy-splash-image "~/.doom.d/seele.png"
      display-line-numbers-type t
      initial-frame-alist '((top . 0.3) (left . 0.45) (width . 80) (height . 40)))
