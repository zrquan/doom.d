;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Some functionality uses this to identify you.
(setq user-full-name "4shen0ne"
      user-mail-address "4shen.01@gmail.com")

(setq auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

(setq doom-localleader-key ","
      doom-localleader-alt-key "M-,")
(map! :leader
      :desc "Translate word" "s w" #'sdcv-search-pointer+
      :desc "Translate input" "s W" #'sdcv-search-input+
      :desc "Google Translate" "s g" #'gts-do-translate
      :desc "Kill buffer & window" "b x" #'kill-buffer-and-window
      :desc "Dirvish sidebar" "o o" #'dirvish-side)

;; Evil
(setq evil-escape-key-sequence "df"
      evil-snipe-override-evil-repeat-keys nil
      evil-vsplit-window-right t
      evil-split-window-below t)

(defun display-ansi-colors ()
  "Display ANSI color codes in current buffer"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; Load separate configs
(load! "+ui")
(load! "+os")
(load! "+text")
