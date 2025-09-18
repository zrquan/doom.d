;;; private/my-chinese/config.el -*- lexical-binding: t; -*-


(use-package! pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :config
  (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t)
  (setq! pangu-spacing-special-region-func-alist
         '((org-mode . pangu-spacing-org-mode-noreal)))
  ;; 不要在node-property中使用real-insert
  (defun pangu-spacing-org-mode-noreal ()
    (let ((element (org-element-at-point)))
      (when (or (pangu-spacing-org-mode-at-special-region)
                (member (org-element-type element) '(node-property)))
        t))))


(use-package! gt
  :config
  (setq! gt-langs '(en zh))
  (setq! gt-default-translator
         (gt-translator
          :engines (list
                    (gt-stardict-engine :if 'word :dir "~/.stardict/dic/" :dict "简明英汉字典增强版")
                    (gt-google-engine :if 'not-word))
          :render  (list
                    (gt-posframe-pop-render :if 'word
                                            :frame-params
                                            (list :border-width 3
                                                  :background-color (doom-color "black")
                                                  :foreground-color (doom-color "white")))
                    (gt-buffer-render)))))
