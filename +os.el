;;; $DOOMDIR/+os.el -*- lexical-binding: t; -*-


;; Socks proxy
(setq! url-gateway-method 'socks
      socks-server '("Default server" "127.0.0.1" 10808 5)
      url-gateway-local-host-regexp
      (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'"))

;; 在terminal使用系统剪贴板
(defadvice gui-backend-set-selection (around set-clip-from-terminal-on-osx activate)
  ad-do-it
  (when (and (equal system-type 'gnu/linux)
             (not (display-graphic-p))
             (not (window-system))
             (equal (ad-get-arg 0) 'CLIPBOARD))
    (let ((process-connection-type nil) ;use pipe
          (default-directory "~/"))
      (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy")))
        (process-send-string proc (ad-get-arg 1))
        (process-send-eof proc)))))

;; 禁止emacsclient打开新的工作区
(after! persp-mode
  (setq! persp-emacsclient-init-frame-behaviour-override -1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! dired-x
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq! dired-omit-files (concat dired-omit-files "\\|^\\..*$")))

(use-package! dirvish
  :defer t
  :init (dirvish-override-dired-mode)
  :config
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (setq! dirvish-side-auto-close t
        dirvish-side-auto-expand nil)
  (setq! dirvish-cache-dir (concat doom-cache-dir "dirvish/")
        dirvish-attributes '(file-size file-time nerd-icons vc-state)
        ;; dirvish-side-width 45
        dirvish-quick-access-entries
        '(("h" "~/" "Home")
          ("d" "~/Downloads/" "Downloads")
          ("o" "~/Dropbox/org/" "Org")
          ("p" "~/Projects/" "Projects")
          ("c" "~/Documents/ctf/" "CTF")
          ("w" "~/Documents/work/" "Work")))
  (map! :map dired-mode-map
        :n "<tab>" #'dirvish-subtree-toggle
        :n "C-h" #'dired-omit-mode
        :n "C-f" #'dirvish-fd
        :n "q" #'dirvish-quit
        :n "b" #'dirvish-quick-access
        :n "s" #'dirvish-quicksort
        :n "z" #'dirvish-history-jump
        :n "f" #'dirvish-file-info-menu
        :n "F" #'dirvish-layout-toggle
        :n "l" #'dired-find-file
        :n "h" #'dired-up-directory))
