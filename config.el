;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Some functionality uses this to identify you.
(setq! user-full-name "4shen0ne"
       user-mail-address "4shen.01@gmail.com")

(setq! org-directory          "~/Dropbox/org/"
       org-roam-directory     "~/Dropbox/org/roam/"
       org-agenda-files     '("~/Dropbox/org/roam/daily/")
       org-hugo-base-dir      "~/Documents/blog/")

(setq! auth-sources '("~/.authinfo" "~/.authinfo.gpg"))

(setq gcmh-high-cons-threshold 1073741824)

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

(after! corfu
  (map! :map corfu-map
        :desc "" "SPC" #'corfu-insert-separator))

(defun zrquan/cve-at-point ()
  "Return the CVE ID at point."
  (let ((word-chars "-A-Za-z0-9"))
    (skip-chars-backward word-chars)
    (let ((start (point)))
      (skip-chars-forward word-chars)
      (let ((end (point)))
        (if (> start end)
            (message "No word at point")
          (let ((word (buffer-substring-no-properties start end)))
            (if (string-match-p "^CVE-[0-9]+-[0-9]+$" (upcase word))
                word
              nil)))))))

(defun zrquan/cnvd-at-point ()
  "Return the CNVD ID at point."
  (let ((word-chars "-A-Za-z0-9"))
    (skip-chars-backward word-chars)
    (let ((start (point)))
      (skip-chars-forward word-chars)
      (let ((end (point)))
        (if (> start end)
            (message "No word at point")
          (let ((word (buffer-substring-no-properties start end)))
            (if (string-match-p "^CNVD-[0-9]+-[0-9]+$" (upcase word))
                word
              nil)))))))

(defun zrquan/display-ansi-colors ()
  "Display ANSI color codes in current buffer"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun zrquan/md-to-org-region (start end)
  "Convert region from markdown to org, replacing selection"
  (interactive "r")
  (shell-command-on-region start end "pandoc -f markdown -t org" t t))

(defun zrquan/nvd-search ()
  "Search CVE Vulnerability."
  (interactive)
  (let ((cve (zrquan/cve-at-point)))
    (if cve
        (let* ((query (url-hexify-string cve))
               (url (concat "https://nvd.nist.gov/vuln/detail/" query)))
          (browse-url url))
      (message "No CVE ID at point"))))

(defun zrquan/cnvd-search ()
  "Search CNVD Vulnerability."
  (interactive)
  (let ((cnvd (zrquan/cnvd-at-point)))
    (if cnvd
        (let* ((query (url-hexify-string cnvd))
               (url (concat "https://www.cnvd.org.cn/flaw/show/" query)))
          (browse-url url))
      (message "No CNVD ID at point"))))

(defun zrquan/reset-amend ()
  "当你提交一个 commit 但不小心使用了 amend 选项时，使用该函数撤回 amend 部分"
  (interactive)
  (magit-reset-soft "HEAD@{1}")
  (magit-commit:--reuse-message "HEAD@{1}"))

;; (after! lsp-mode
;;   ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
;;   (delete 'lsp-terraform lsp-client-packages))

(use-package! gitmoji
  :commands (gitmoji-insert))

(use-package! verb
  :config (progn
            (setq verb-trim-body-end "[ \t\n\r]+")
            (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))

;; Load separate configs
(load! "+ui")
(load! "+os")
(load! "+text")
