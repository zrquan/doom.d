;;; input/chinese/config.el -*- lexical-binding: t; -*-

(use-package! pangu-spacing
  :hook (text-mode . pangu-spacing-mode)
  :config
  (setq-hook! 'org-mode-hook pangu-spacing-real-insert-separtor t)
  (setq pangu-spacing-special-region-func-alist
        '((org-mode . pangu-spacing-org-mode-noreal)))
  ;; 在某些位置不插入空格字符
  (defun pangu-spacing-org-mode-noreal ()
    (let ((element (org-element-at-point)))
      (when (or (pangu-spacing-org-mode-at-special-region)
                (member (org-element-type element) '(node-property)))
        t))))


(use-package! go-translate
  :config
  (setq gts-translate-list '(("en" "zh"))
        gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine))
         :render (gts-buffer-render))
        gts-posframe-pop-render-timeout 999)
  ;; 翻译时消除换行符以提高准确度
  (cl-defmethod gts-translate :before ((o gts-engine) task callback)
    (with-slots (text) task
      (setf text (replace-regexp-in-string "[ \t\n]+" " " text)))))


(use-package! sdcv
  :config
  (setq sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic/")
        sdcv-dictionary-simple-list '("简明英汉字典增强版")
        sdcv-dictionary-complete-list '("简明英汉字典增强版")))


(use-package! rime
  :custom (default-input-method "rime")
  :config
  (setq rime-share-data-dir "~/.config/ibus/rime/"
        rime-translate-keybindings '("C-g" "C-j" "C-k" "C-," "C-.")
        rime-show-candidate 'posframe
        rime-posframe-style 'vertical
        rime-posframe-properties '(:internal-border-width 2)))

;;; end
