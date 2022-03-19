;;; input/chinese/config.el -*- lexical-binding: t; -*-

(use-package! pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :config
  (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t)

  ;; 在某些位置不插入空格字符
  (defun pangu-spacing-org-mode-noreal ()
    (let ((element (org-element-at-point)))
      (when (member (org-element-type element) '(node-property))
        t)))
  (push '(org-mode . pangu-spacing-org-mode-noreal)
        pangu-spacing-special-region-func-alist))

(use-package! go-translate
  :config
  (setq gts-translate-list '(("en" "zh")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine))
         :render (gts-posframe-pop-render))))

(use-package! youdao-dictionary
  :config
  (map! :map youdao-dictionary-mode-map
        :n "q" #'quit-window))

(use-package! liberime
  :init
  (progn
    (setq liberime-module-file (concat doom-local-dir "liberime-core.so")
          liberime-user-data-dir (file-truename "~/.doom.d/modules/input/chinese/rime"))
    (liberime-load))
  :config
  (liberime-select-schema "double_pinyin_mspy")
  (add-hook 'after-init-hook #'liberime-sync))

(use-package! pyim
  :after liberime
  :init
  (setq pyim-dcache-directory (concat doom-cache-dir "pyim/"))
  :config
  (setq default-input-method "pyim")
  (if (load-library "pyim-liberime")
      (setq pyim-default-scheme 'rime-microsoft-shuangpin)
    (setq pyim-default-scheme 'microsoft-shuangpin))

  ;; 兼容`evil-escape'
  (defun my-pyim-self-insert-command (orig-func)
    (interactive "*")
    (if (and (local-variable-p 'last-event-time)
             (floatp last-event-time)
             (< (- (float-time) last-event-time) 0.2))
        (set (make-local-variable 'temp-evil-escape-mode) t)
      (set (make-local-variable 'temp-evil-escape-mode) nil))
    (if (and temp-evil-escape-mode
             (equal (pyim-entered-get) "d")
             (equal last-command-event ?f))
        (progn
          (push last-command-event unread-command-events)
          (pyim-process-outcome-handle 'pyim-entered)
          (pyim-process-terminate))
      (progn
        (call-interactively orig-func)
        (set (make-local-variable 'last-event-time) (float-time)))))
  (advice-add 'pyim-self-insert-command :around #'my-pyim-self-insert-command)

  ;; 中/英文自动切换
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-auto-english
                  pyim-probe-isearch-mode
                  pyim-probe-org-structure-template))

  ;; 全/半角标点自动切换
  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  (global-set-key (kbd "C-x \\") 'pyim-convert-string-at-point)
  (define-key pyim-mode-map "." 'pyim-page-next-page)
  (define-key pyim-mode-map "," 'pyim-page-previous-page))

;;; end
