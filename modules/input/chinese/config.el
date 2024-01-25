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
        rime-show-candidate 'minibuffer
        rime-disable-predicates '(rime-predicate-evil-mode-p
                                  rime-predicate-current-uppercase-letter-p
                                  rime-predicate-prog-in-code-p))

  ;; 结合 evil-escape 一起使用
  (defun rime-evil-escape-advice (orig-fun key)
    "advice for `rime-input-method' to make it work together with `evil-escape'.
        Mainly modified from `evil-escape-pre-command-hook'"
    (if rime--preedit-overlay
        ;; if `rime--preedit-overlay' is non-nil, then we are editing something, do not abort
        (apply orig-fun (list key))
      (when (featurep 'evil-escape)
        (let (
              (fkey (elt evil-escape-key-sequence 0))
              (skey (elt evil-escape-key-sequence 1))
              )
          (if (or (char-equal key fkey)
                  (and evil-escape-unordered-key-sequence
                       (char-equal key skey)))
              (let ((evt (read-event nil nil evil-escape-delay)))
                (cond
                 ((and (characterp evt)
                       (or (and (char-equal key fkey) (char-equal evt skey))
                           (and evil-escape-unordered-key-sequence
                                (char-equal key skey) (char-equal evt fkey))))
                  (evil-repeat-stop)
                  (evil-normal-state))
                 ((null evt) (apply orig-fun (list key)))
                 (t
                  (apply orig-fun (list key))
                  (if (numberp evt)
                      (apply orig-fun (list evt))
                    (setq unread-command-events (append unread-command-events (list evt))))))
                )
            (apply orig-fun (list key)))))))

  (advice-add 'rime-input-method :around #'rime-evil-escape-advice))

;;; end
