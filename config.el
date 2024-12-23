;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Some functionality uses this to identify you.
(setq! user-full-name "4shen0ne"
       user-mail-address "4shen.01@gmail.com")

(setq! org-directory          "~/Dropbox/org/"
       org-roam-directory     "~/Dropbox/org/roam/"
       org-agenda-files     '("~/Dropbox/org/todo.org" "~/Dropbox/org/roam/daily/")
       org-hugo-base-dir      "~/Documents/blog/")

(setq tab-width 4
      gcmh-high-cons-threshold (* 128 1024 1024)) ;128MB

(setq! doom-localleader-key ","
       doom-localleader-alt-key "M-,")

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

(use-package! igist
  :config
  (setq igist-auth-marker 'igist))

;; Load separate configs
(load! "+ui")
(load! "+os")
(load! "+org")

;; Keyboard mappings
;; Global
(map! :leader
      :desc "Switch buffer" "." #'consult-buffer
      :desc "Switch to last buffer" "<" #'evil-switch-to-windows-last-buffer
      :desc "Kill buffer & window" "b x" #'kill-buffer-and-window
      :desc "Go translate" "s g" #'gt-do-translate
      :desc "Dirvish sidebar" "o o" #'dirvish-side
      :desc "Capture today" "n n" #'org-roam-dailies-capture-today
      :desc "Goto date" "n N" (lambda ()
                                (interactive)
                                (org-roam-dailies-goto-date nil "d")))
(map! :map org-mode-map
      :localleader
      :desc "org-emphasize" "X" #'org-emphasize)
;; dirvish
(map! :after dirvish
      :map dired-mode-map
      :n "<tab>" #'dirvish-subtree-toggle
      :n "C-h" #'dired-omit-mode
      :n "C-f" #'dirvish-fd-ask
      :n "q" #'dirvish-quit
      :n "b" #'dirvish-quick-access
      :n "s" #'dirvish-quicksort
      :n "z" #'dirvish-history-jump
      :n "f" #'dirvish-file-info-menu
      :n "F" #'dirvish-layout-toggle
      :n "l" #'dired-find-file
      :n "h" #'dired-up-directory)
;; verb
(map! :map org-mode-map
      :localleader
      :prefix ("v" . "verb")
      :desc "send request stay" "r" #'verb-send-request-on-point-other-window-stay
      :desc "send request" "s" #'verb-send-request-on-point-other-window
      :desc "show vars" "x" #'verb-show-vars
      :desc "kill response buffers" "k" #'verb-kill-all-response-buffers)

;; 像素级滚动
;; https://emacs-china.org/t/topic/25114/5
(pixel-scroll-precision-mode 1)
(setq pixel-scroll-precision-interpolate-page t)
(defun +pixel-scroll-interpolate-down (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* -1 lines (pixel-line-height)))
    (pixel-scroll-interpolate-down)))

(defun +pixel-scroll-interpolate-up (&optional lines)
  (interactive)
  (if lines
      (pixel-scroll-precision-interpolate (* lines (pixel-line-height))))
  (pixel-scroll-interpolate-up))

(defalias 'scroll-up-command '+pixel-scroll-interpolate-down)
(defalias 'scroll-down-command '+pixel-scroll-interpolate-up)
