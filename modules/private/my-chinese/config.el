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


(use-package! go-translate
  :defer t
  :config
  (setq! gt-langs '(en zh))
  (setq! gt-default-translator
         (gt-translator
          :engines (list (gt-google-engine :if 'not-word) (gt-youdao-dict-engine :if 'word))
          :render  (gt-posframe-pop-render))))


(use-package! sdcv
  :defer t
  :config
  (setq! sdcv-dictionary-data-dir (expand-file-name "~/.stardict/dic/")
        sdcv-dictionary-simple-list '("简明英汉字典增强版")))


;; (use-package! rime
;;   :defer t
;;   :custom (default-input-method "rime")
;;   :config
;;   (setq! rime-share-data-dir "~/.local/share/fcitx5/rime/"
;;         rime-translate-keybindings '("C-g" "C-j" "C-k" "C-," "C-.")
;;         rime-show-candidate 'posframe
;;         rime-disable-predicates '(rime-predicate-evil-mode-p
;;                                   rime-predicate-current-uppercase-letter-p
;;                                   rime-predicate-punctuation-line-begin-p
;;                                   rime-predicate-prog-in-code-p))

;;   ;; 结合evil-escape一起使用
;;   (defun rime-evil-escape-advice (orig-fun key)
;;     "advice for `rime-input-method' to make it work together with `evil-escape'.
;;         Mainly modified from `evil-escape-pre-command-hook'"
;;     (if rime--preedit-overlay
;;         ;; if `rime--preedit-overlay' is non-nil, then we are editing something, do not abort
;;         (apply orig-fun (list key))
;;       (when (featurep 'evil-escape)
;;         (let (
;;               (fkey (elt evil-escape-key-sequence 0))
;;               (skey (elt evil-escape-key-sequence 1))
;;               )
;;           (if (or (char-equal key fkey)
;;                   (and evil-escape-unordered-key-sequence
;;                        (char-equal key skey)))
;;               (let ((evt (read-event nil nil evil-escape-delay)))
;;                 (cond
;;                  ((and (characterp evt)
;;                        (or (and (char-equal key fkey) (char-equal evt skey))
;;                            (and evil-escape-unordered-key-sequence
;;                                 (char-equal key skey) (char-equal evt fkey))))
;;                   (evil-repeat-stop)
;;                   (evil-normal-state))
;;                  ((null evt) (apply orig-fun (list key)))
;;                  (t
;;                   (apply orig-fun (list key))
;;                   (if (numberp evt)
;;                       (apply orig-fun (list evt))
;;                     (setq! unread-command-events (append unread-command-events (list evt))))))
;;                 )
;;             (apply orig-fun (list key)))))))

;;   (advice-add 'rime-input-method :around #'rime-evil-escape-advice))
