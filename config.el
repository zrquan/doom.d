;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Some functionality uses this to identify you.
(setq! user-full-name "4shen0ne"
       user-mail-address "4shen.01@gmail.com")

(setq! org-directory          "~/Dropbox/org/"
       org-roam-directory     "~/Dropbox/org/roam/"
       org-agenda-files     '("~/Dropbox/org/todo.org" "~/Dropbox/org/roam/daily/")
       org-hugo-base-dir      "~/Documents/blog/"
       org-attach-id-dir      "~/Documents/org-attach/")

(setq tab-width 4
      warning-minimum-level :error
      gcmh-high-cons-threshold (* 1024 1024 1024)  ;1GB
      gc-cons-percentage 0.3
      vc-handled-backends '(Git)
      delete-by-moving-to-trash t)

(setq! doom-localleader-key ","
       doom-localleader-alt-key "M-,")
(define-key evil-motion-state-map (kbd ",") #'doom/leader)

;; Evil
(setq! evil-escape-key-sequence "df"
       evil-snipe-override-evil-repeat-keys nil
       evil-vsplit-window-right t
       evil-split-window-below t
       evil-disable-insert-state-bindings t)

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

(defun +default/org-notes-headlines ()
  "Jump to an Org headline in `org-agenda-files'."
  (interactive)
  (doom-completing-read-org-headings
   "Jump to org headline: " org-agenda-files
   :depth 1
   :include-files t))

(defun xah-open-in-vscode ()
  "Open current file or dir in vscode.
URL `http://xahlee.info/emacs/emacs/emacs_open_in_vscode.html'

Version: 2020-02-13 2021-01-18 2022-08-04 2023-06-26"
  (interactive)
  (let ((xpath (if buffer-file-name buffer-file-name (expand-file-name default-directory))))
    (message "path is %s" xpath)
    (cond
     ((eq system-type 'darwin)
      (shell-command (format "open -a Visual\\ Studio\\ Code.app %s" (shell-quote-argument xpath))))
     ((eq system-type 'windows-nt)
      (shell-command (format "code.cmd %s" (shell-quote-argument xpath))))
     ((eq system-type 'gnu/linux)
      (shell-command (format "code %s" (shell-quote-argument xpath)))))))

;; (after! lsp-mode
;;   ;; https://github.com/emacs-lsp/lsp-mode/issues/3577#issuecomment-1709232622
;;   (delete 'lsp-terraform lsp-client-packages))

(after! eglot
  (setq! eglot-ignored-server-capabilities
         '(:hoverProvider
           :documentHighlightProvider
           :documentOnTypeFormattingProvider
           :colorProvider
           :foldingRangeProvider)))

(use-package! gitmoji
  :commands (gitmoji-insert))

;; (use-package! igist
;;   :config
;;   (setq igist-auth-marker 'igist))

;; Load separate configs
(load! "+ui")
(load! "+os")
(load! "+org")

;; Keyboard mappings
;; Global
(map! :leader
      :desc "Visual fill column" "t v" #'visual-line-fill-column-mode
      :desc "Switch buffer" "." #'consult-buffer
      :desc "Switch to last buffer" "<" #'evil-switch-to-windows-last-buffer
      :desc "Kill buffer & window" "b x" #'kill-buffer-and-window
      :desc "Go translate" "s g" #'gt-translate
      :desc "Copy link" "s L" #'link-hint-copy-link
      :desc "Prodigy" "P" #'prodigy
      :desc "Capture today" "n n" #'org-roam-dailies-capture-today
      :desc "Goto date" "n N" (lambda ()
                                (interactive)
                                (org-roam-dailies-goto-date nil "d")))
;; fold
;; (map! (:when (modulep! :tools tree-sitter)
;;         :n "z m" #'ts-fold-close-all
;;         :n "z r" #'ts-fold-open-all
;;         :n "z a" #'ts-fold-toggle
;;         :n "z O" #'ts-fold-open-recursively))

;; 参考 https://hutusi.com/articles/git-paging 实现 git 历史的「翻页」功能
(defun zrquan/git-first-or-last (&optional last-commit)
  "Check out the first (or last) commit in the default remote branch."
  (interactive "P")
  (let* ((branch (string-trim (shell-command-to-string "git symbolic-ref refs/remotes/origin/HEAD")))
         (opt (if last-commit "" "--reverse"))
         (first-commit (string-trim (shell-command-to-string
                                     (format "git log %s --pretty=%%H %s | head -1" opt branch)))))
    (if (and branch first-commit (not (string-empty-p first-commit)))
        (progn
          (shell-command (format "git checkout %s" first-commit))
          (revert-buffer :ignore-auto :noconfirm))
      (error "Could not determine the first commit or branch"))))

(defun zrquan/git-next (&optional n)
  "Check out the Nth next commit from the current HEAD in the default remote branch.
If N is not provided, it defaults to 1."
  (interactive "P")
  (let* ((branch (string-trim (shell-command-to-string "git symbolic-ref refs/remotes/origin/HEAD")))
         (current-commit (string-trim (shell-command-to-string "git rev-parse HEAD")))
         (num (or (and n (prefix-numeric-value n)) 1))
         (next-commit (string-trim (shell-command-to-string
                                    (format "git log --reverse --pretty=%%H %s | grep -A %d %s | tail -1"
                                            branch num current-commit)))))
    (if (and branch current-commit next-commit (not (string-empty-p next-commit)))
        (progn
          (shell-command (format "git checkout %s" next-commit))
          (revert-buffer :ignore-auto :noconfirm)
          (message "Checked out next commit: %s" next-commit))
      (error "Could not determine the next commit"))))

(defun zrquan/git-prev (&optional n)
  "Check out the Nth previous commit from the current HEAD.
If N is not provided, it defaults to 1."
  (interactive "P")
  (let* ((num (or (and n (prefix-numeric-value n)) 1))
         (command (format "git checkout HEAD~%d" num)))
    (if (zerop (shell-command command))
        (progn
          (message "Checked out previous commit: HEAD~%d" num)
          (revert-buffer :ignore-auto :noconfirm))
      (error "Failed to check out previous commit"))))

;; FIXME: md-file is nil
;; (defun zrquan/delete-org-and-hugo-md ()
;;   "删除当前 org 文件时，一并删除 `ox-hugo' 导出的 md 文件"
;;   (interactive)
;;   (let (md-file (org-hugo-export-to-md))
;;     (progn
;;       (doom/delete-this-file md-file)
;;       (doom/delete-this-file))))
