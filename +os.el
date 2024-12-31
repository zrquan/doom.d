;;; $DOOMDIR/+os.el -*- lexical-binding: t; -*-


;; Socks proxy
;; (setq! url-gateway-method 'socks
;;       socks-server '("Default server" "127.0.0.1" 10808 5)
;;       url-gateway-local-host-regexp
;;       (concat "\\`" (regexp-opt '("localhost" "127.0.0.1")) "\\'"))

;; 在terminal使用系统剪贴板
(defadvice gui-backend-set-selection (around set-clip-from-terminal-on-osx activate)
  ad-do-it
  (when (and (equal system-type 'gnu/linux)
             (not (display-graphic-p))
             (not (window-system))
             (equal (ad-get-arg 0) 'CLIPBOARD))
    (let ((process-connection-type nil) ;use pipe
          (default-directory "~/"))
      (let ((proc (start-process "xclip" "*Messages*" "xclip" "-sel" "clip")))
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

(setq! dired-listing-switches "-Al -X")
(use-package! dirvish
  :defer t
  :init (dirvish-override-dired-mode)
  :config
  (add-hook 'dired-mode-hook 'dired-omit-mode)
  (add-to-list 'dirvish-preview-disabled-exts "gif")
  (setq! dirvish-attributes '(file-size file-time nerd-icons vc-state)
         dirvish-side-auto-close t
         dirvish-side-auto-expand nil
         dirvish-side-follow-mode t
         dirvish-quick-access-entries
         '(("h" "~/" "Home")
           ("d" "~/Downloads/" "Downloads")
           ("o" "~/Dropbox/org/" "Org")
           ("p" "~/Projects/" "Projects")
           ("c" "~/CTF/" "CTF")
           ("a" "~/Armory/" "Armory")
           ("w" "~/Documents/work/" "Work"))))

(after! prodigy
  (prodigy-define-service
    :name "quartz"
    :command "npx"
    :args '("quartz" "build" "--serve")
    :url "http://localhost:8080"
    :cwd "~/Documents/notes/"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "hugo"
    :command "hugo"
    :args '("server" "-D")
    :url "http://localhost:1313"
    :cwd "~/Documents/blog/"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))
