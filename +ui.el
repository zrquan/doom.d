;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-
;; Package-Requires: ((dash "2.19.1"))


(display-time-mode)
(display-battery-mode)

;; Make `doom-variable-pitch-font' and `doom-font' have the same font, otherwise
;; there will be problems with Chinese scaling.
(setq! doom-font (font-spec :family "LXGW WenKai Mono" :size 32)
       doom-symbol-font (font-spec :family "LXGW WenKai Mono")
       doom-variable-pitch-font doom-font)

(require 'dash)
(setq default-frame-alist
      ;; require `dash.el'
      (-union default-frame-alist
              '((top . 0.2) (left . 0.45) (width . 120) (height . 43))))

(use-package! catppuccin-theme
  :config
  (setq! catppuccin-height-doc-title 1.3
         catppuccin-height-title-1 1.1
         catppuccin-height-title-2 1.0
         catppuccin-height-title-3 1.0))

(setq! doom-theme 'doom-gruvbox-light
       doom-modeline-icon t
       fancy-splash-image "~/.doom.d/seele.png"
       display-line-numbers-type t
       initial-frame-alist '((top . 0.2) (left . 0.45) (width . 120) (height . 43)))

(after! vertico
  (vertico-indexed-mode 1)
  (setq! vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center))
