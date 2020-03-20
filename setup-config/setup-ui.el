;; UI & Appearance

;;; Scrolling
(setq auto-window-vscroll nil)
;; the text cursor moves off-screen. Instead, only scroll the minimum amount
;; necessary to show the new line. (A number of 101+ disables re-centering.)
(setq scroll-conservatively 101)

;;; Fonts
;;(defvar cpm-font1 (font-spec :family "InconsolataLGC Nerd Font" :size 13))
(defvar cpm-font1 (font-spec :family "Menlo" :size 13))
(defvar cpm-font2 (font-spec :family "Hasklug Nerd Font" :size 13))
(defvar cpm-font3 (font-spec :family "DejaVuSansMono Nerd Font" :size 13))
(defvar cpm-font4 (font-spec :family "SauceCodePro Nerd Font" :size 13))
(defvar cpm-ligatures nil)
;; (defvar cpm-vari-font (font-spec :family "Avenir"))
;; (defvar cpm-unicode-font (font-spec :family "Symbola"))
(set-face-attribute 'default nil :font cpm-font1)
;; (set-face-attribute 'variable-pitch nil :font cpm-vari-font)
;; (set-fontset-font t 'unicode cpm-unicode-font nil 'prepend)
(setq-default line-spacing 0.10)

;;; Scale Text
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)

;;; Line Numbers
(use-package display-line-numbers
  :ensure nil
  ;; :hook (markdown-mode prog-mode)
  :commands display-line-numbers-mode
  :init
  (setq-default display-line-numbers-type 'visual))

(use-package hl-todo
  :defer t
  :commands hl-todo-mode
  :init
  ;; (add-hook 'org-mode-hook #'hl-todo-mode)
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-ui)
