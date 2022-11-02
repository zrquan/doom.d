;;; completion/corfu/config.el -*- lexical-binding: t; -*-

;; Corfu completion module


;; Reset lsp-completion provider
(add-hook 'doom-init-modules-hook
          (lambda ()
            (after! lsp-mode
              (setq lsp-completion-provider :none))))

;; Pad before lsp modeline error info
(add-hook 'lsp-mode-hook
          (lambda ()
            (setf (caadr
                   (assq 'global-mode-string mode-line-misc-info))
                  " ")))

;; Set orderless filtering for LSP-mode completions
(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless))))))

;; Fallback cleanly to consult in TUI
(setq-default completion-in-region-function #'consult-completion-in-region)

(use-package corfu
  :custom
  (corfu-separator ?\s)          ;; Orderless field separator
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-auto t)
  (corfu-on-exact-match nil)
  (corfu-quit-no-match t)
  (corfu-excluded-modes '(org-mode))
  :hook
  (doom-first-buffer . global-corfu-mode)
  :bind (:map corfu-map
         ("SPC" . corfu-insert-separator)
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous))
  :config
  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map))

(use-package orderless
  :when (modulep! +orderless)
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package kind-all-the-icons
  :when (modulep! +icon)
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))

(use-package cape
  :defer t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(setq completion-cycle-threshold 1)

;; Enable indentation+completion using the TAB key.
;; Completion is often bound to M-TAB.
(setq tab-always-indent 'complete)

;; Dirty hack to get c completion running
;; Discussion in https://github.com/minad/corfu/issues/34
(when (equal tab-always-indent 'complete)
  (map! :map c-mode-base-map
        :i [remap c-indent-line-or-region] #'completion-at-point))
