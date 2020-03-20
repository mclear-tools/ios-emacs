;;; Avy
(use-package avy
  :commands (avy-goto-char))

;;; Imenu list outline
(use-package imenu-list
  :ensure t
  :commands (imenu-list-smart-toggle imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-position 'left))

;;; Save place
(use-package saveplace
  :init
  (save-place-mode 1)
  :config
  (setq save-place-file (concat cpm-cache-dir "saved-places"))
  (setq save-place-forget-unreadable-files nil))

;;; Go To Change
(use-package goto-chg
  :ensure t
  :commands goto-last-change goto-last-change-reverse)

;; ;;; Hydra
(use-package hydra :defer 1)

;; hydra for TODOs
(with-eval-after-load 'hydra
  (defhydra cpm/hydra-todo
    (:pre
     (hl-todo-mode 1)
     :post
     (hl-todo-mode -1))
    "Todo"
    ("n" hl-todo-next "Next")
    ("p" hl-todo-previous "Previous")
    ("o" hl-todo-occur "Occur")
    ("q" nil "Quit" :color blue :exit t)))

;; ;;; Recent files
(use-package recentf
  :ensure nil
  :commands (helm-recentf)
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-save-file (concat cpm-cache-dir "recentf"))
  ;; remove agenda files from list.
  (setq recentf-exclude '("writing.org"
                          "inbox.org"
                          "todo.org"
                          "teaching.org"
                          "someday.org"
                          "bookmark"
                          "elpa"
                          "cache")
        recentf-max-saved-items 300
        recentf-max-menu-items 10))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-navigation)
