;;; $DOOMDIR/+ui.el -*- lexical-binding: t; -*-
;; Package-Requires: ((dash "2.19.1"))


;; Make `doom-variable-pitch-font' and `doom-font' have the same font, otherwise
;; there will be problems with Chinese scaling.
(setq! doom-font (font-spec :family "LXGW WenKai Mono" :size 32)
       doom-symbol-font (font-spec :family "LXGW WenKai Mono")
       doom-variable-pitch-font doom-font
       line-spacing 0.1
       +fold-ellipsis "  ")

(require 'dash)
(setq default-frame-alist
      ;; require `dash.el'
      (-union default-frame-alist
              '((top . 0.23) (left . 0.42) (width . 100) (height . 43) (alpha . 97))))

;; (use-package! catppuccin-theme
;;   :config
;;   (setq! catppuccin-height-doc-title 1.3
;;          catppuccin-height-title-1 1.1
;;          catppuccin-height-title-2 1.0
;;          catppuccin-height-title-3 1.0))

(setq! doom-theme 'doom-tokyo-night
       fancy-splash-image "~/.doom.d/door-white.svg"
       doom-modeline-icon t
       doom-modeline-buffer-file-name-style 'truncate-with-project
       display-line-numbers-type t)

(after! vertico
  (vertico-indexed-mode 1)
  (setq! vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center))
