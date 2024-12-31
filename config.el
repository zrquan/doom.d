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

(defun zrquan/display-ansi-colors ()
  "Display ANSI color codes in current buffer"
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(defun zrquan/md-to-org-region (start end)
  "Convert region from markdown to org, replacing selection"
  (interactive "r")
  (shell-command-on-region start end "pandoc -f markdown -t org" t t))

(defun zrquan/search-common-vulnerability ()
  "查看 CVE/CNVD 官方漏洞信息"
  (interactive)
  (let* ((word (thing-at-point 'symbol))
         (cve-regexp "^CVE-\\([0-9]\\{4\\}\\)-\\([0-9]\\{4,\\}\\)$")
         (cnvd-regexp "^CNVD-\\([0-9]\\{4\\}\\)-\\([0-9]\\{4,\\}\\)$")
         (cve-id (and (string-match cve-regexp word) word))
         (cnvd-id (and (string-match cnvd-regexp word) word)))
    (cond
     (cve-id
      (browse-url (format "https://nvd.nist.gov/vuln/detail/%s" cve-id)))
     (cnvd-id
      (browse-url (format "https://www.cnvd.org.cn/flaw/show/%s" cnvd-id)))
     (t
      (message "当前光标不在 CVE/CNVD 编号上")))))

(defun zrquan/reset-amend ()
  "当你提交一个 commit 但不小心使用了 amend 选项时，使用该函数撤回 amend 部分"
  (interactive)
  (magit-reset-soft "HEAD@{1}")
  (magit-commit:--reuse-message "HEAD@{1}"))

(defun get-current-graphic-system ()
  "Determine the current graphic system (X11 or Wayland)."
  (cond
   ((getenv "WAYLAND_DISPLAY")
    "Wayland")
   ((getenv "DISPLAY")
    "X11")
   (t
    "Unknown")))

(defun zrquan/dired-copy-png ()
  "Copy the PNG file at point in dired to the clipboard."
  (interactive)
  (let ((file (dired-get-file-for-visit))
        (graphic-system (get-current-graphic-system)))
    (when (and file (string-match-p "\\.png\\'" file))
      (cond
       ((string= graphic-system "Wayland")
        (with-temp-buffer
          (insert-file-contents file)
          (call-process-region (point-min) (point-max) "wl-copy"))
        (message "Copied %s to clipboard using wl-copy" file))
       ((string= graphic-system "X11")
        (shell-command (format "xclip -selection clipboard -t image/png -i %s" file))
        (message "Copied %s to clipboard using xclip" file))
       (t
        (message "Unknown graphic system"))))))
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "C-c C-p") 'zrquan/dired-copy-png)))

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
      :desc "Copy link" "s L" #'link-hint-copy-link
      :desc "Dirvish sidebar" "o o" #'dirvish-side
      :desc "Capture today" "n n" #'org-roam-dailies-capture-today
      :desc "Goto date" "n N" (lambda ()
                                (interactive)
                                (org-roam-dailies-goto-date nil "d")))
(map! :map org-mode-map
      :localleader
      :desc "org-emphasize" "X" #'org-emphasize
      :desc "org-download-delete" "a D" #'org-download-delete)
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
