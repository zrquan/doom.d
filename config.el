;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Some functionality uses this to identify you.
(setq! user-full-name "4shen0ne"
       user-mail-address "4shen.01@gmail.com")

(setq! auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

(setq! doom-localleader-key ","
       doom-localleader-alt-key "M-,")
(map! :leader
      :desc "Kill buffer & window" "b x" #'kill-buffer-and-window
      :desc "Dirvish sidebar" "o o" #'dirvish-side
      (:when (modulep! :private my-chinese)
        :leader
        :desc "sdcv-search-pointer+" "s w" #'sdcv-search-pointer+
        :desc "sdcv-search-input+" "s W" #'sdcv-search-input+
        :desc "gts-do-translate" "s g" #'gt-do-translate)
      (:when (modulep! :lang org +roam2)
        :leader
        :desc "Capture today" "n n" #'org-roam-dailies-capture-today
        :desc "Goto date" "n N" (lambda ()
                                  (interactive)
                                  (org-roam-dailies-goto-date nil "d"))))

;; Evil
(setq! evil-escape-key-sequence "df"
       evil-snipe-override-evil-repeat-keys nil
       evil-vsplit-window-right t
       evil-split-window-below t)

(defun zrq/display-ansi-colors ()
  "Display ANSI color codes in current buffer"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun zrq/md-to-org-region (start end)
  "Convert region from markdown to org, replacing selection"
  (interactive "r")
  (shell-command-on-region start end "pandoc -f markdown -t org" t t))

(defun zrq/nvd-search ()
  "Search NVD for the selected text."
  (interactive)
  (if (use-region-p)
      (let* ((selection (buffer-substring-no-properties (region-beginning) (region-end)))
             (query (url-hexify-string selection))
             (url (concat "https://nvd.nist.gov/vuln/detail/" query)))
        (browse-url url))
    (message "No text selected.")))

(defun zrq/reset-amend ()
  "当你提交一个 commit 但不小心使用了 amend 选项时，使用该函数撤回 amend 部分"
  (interactive)
  (magit-reset-soft "HEAD@{1}")
  (magit-commit:--reuse-message "HEAD@{1}"))

;; (after! lsp-mode
;;   ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
;;   (delete 'lsp-terraform lsp-client-packages))

;; Load separate configs
(load! "+ui")
(load! "+os")
(load! "+text")
