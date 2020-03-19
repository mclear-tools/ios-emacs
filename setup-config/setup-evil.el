;;; Evil Mode

(use-package evil
  :defer .1 ;; don't block emacs when starting, load evil immediately after startup
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)
  :config
  (add-hook 'git-commit-mode-hook 'evil-insert-state)
  (evil-set-initial-state 'dashboard-mode 'motion)
  (evil-set-initial-state 'messages-buffer-mode 'motion)
  (evil-set-initial-state 'magit-log-edit-mode 'insert)
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-set-initial-state 'org-export-dispatch 'motion)
  ;; evil-normal-state is preferred, so revert when idle
  (run-with-idle-timer 60 t 'evil-normal-state)
  ;; don't echo evil state
  (setq evil-echo-state nil)
  ;; don't move cursor back when exiting insert state
  (setq evil-move-cursor-back nil)
  ;; evil everywhere
  (evil-mode 1))

;;; Evil Collection
(use-package evil-collection
  :ensure t
  :after evil
  :hook (evil-after-load-hook . evil-collection-init)
  :custom (evil-collection-company-use-tng nil)
  :init
  (evil-collection-init))

;;; Evil Surround
(use-package evil-surround
  :defer 1
  :commands (evil-surround-region evil-surround-change evil-surround-delete)
  :general
  (:states '(visual)
   "s" 'evil-surround-region
   "S" 'evil-substitute)
  :config (global-evil-surround-mode 1))

(use-package evil-embrace
  :after evil-surround
  :demand t
  :hook ((markdown org) . embrace-org-mode-hook)
  :config
  (with-eval-after-load 'evil-surround
    (evil-embrace-enable-evil-surround-integration))
  (setq evil-embrace-show-help-p nil)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (defun embrace-markdown-mode-hook ()
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  (add-hook 'markdown-mode-hook 'embrace-markdown-mode-hook))

;;; Evil Comments
(use-package evil-commentary
  :commands (evil-commentary evil-commentary-line)
  :config
  (evil-commentary-mode))

;;; Evil Undo
;; there are problems but there doesn't seem to be a workaround
;; https://github.com/emacs-evil/evil/issues/1074 and
;; http://ergoemacs.org/emacs/emacs_best_redo_mode.html
(use-package undo-tree
  ;; :commands (undo-tree-undo undo-tree-redo undo-tree-visualize)
  :init
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-enable-undo-in-region nil)
  ;; supposedly causes errors in undo read
  ;; see https://emacs.stackexchange.com/a/34214/11934
  (setq undo-tree-enable-undo-in-region nil)
  ;; stop littering - set undo directory
  (let ((undo-dir (concat cpm-cache-dir "undo")))
    (setq undo-tree-history-directory-alist `(("." . ,undo-dir)))
    (unless (file-directory-p undo-dir)
      (make-directory undo-dir t)))
  (setq undo-tree-auto-save-history nil)
  :config
  (global-undo-tree-mode 1))


;;; End Evil-Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'setup-evil)

