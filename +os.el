;;; $DOOMDIR/+os.el -*- lexical-binding: t; -*-


;; 禁止emacsclient打开新的工作区
(after! persp-mode
  (setq! persp-emacsclient-init-frame-behaviour-override -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! projectile
  (setq projectile-cache-file (concat doom-cache-dir "projectile-cache.eld")))

(after! dired-x
  ;; Make dired-omit-mode hide all "dotfiles"
  (setq! dired-omit-files (concat dired-omit-files "\\|^\\..*$")))

(after! dirvish
  (setq! dirvish-default-layout '(0 0 0.65)
         dirvish-attributes '(file-size file-time nerd-icons vc-state)
         dirvish-side-auto-close t
         dirvish-quick-access-entries
         '(("h" "~/" "Home")
           ("d" "~/Downloads/" "Downloads")
           ("o" "~/Dropbox/org/" "Org")
           ("p" "~/Projects/" "Projects")
           ("c" "~/CTF/" "CTF")
           ("a" "~/Armory/" "Armory")
           ("w" "~/Documents/work/" "Work")))
  (add-to-list 'dirvish-preview-disabled-exts "gif")
  (map! :map dirvish-mode-map
        :n "C-h" #'dired-omit-mode
        :n "C-f" #'dirvish-fd))

(use-package! prodigy
  :init
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
    :kill-process-buffer-on-stop t)
  (prodigy-define-service
    :name "payloads"
    :command "uv"
    :args '("run" "mkdocs" "serve" "-o")
    :url "http://127.0.0.1:8000/PayloadsAllTheThings/"
    :cwd "~/SecTools/Docs/pat/"
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(use-package! popper
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*DeepSeek\\*"
          "\\*gt-result\\*"
          "\\*Occur\\*"
          "\\*HTTP Response.*\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)
  :config
  (map! :leader
        :desc "Popper" "p p" #'popper-toggle
        :desc "Popper type" "p t" #'popper-toggle-type
        :desc "Popper cycle" "p <tab>" #'popper-cycle
        :desc "Popper kill" "p k" #'popper-kill-latest-popup
        :desc "Switch project" "p P" #'projectile-switch-project))
