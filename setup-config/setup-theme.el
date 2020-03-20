;; Themes
;; I use solarized except in terminal, where Gruvbox seems to work better

;;; Gruvbox Theme
(use-package gruvbox-theme
  :if (not (display-graphic-p))
  :demand t
  :config
  (load-theme 'gruvbox t))

;;; Packaging Themes

;; (defvar packages-appearance '(doom-themes
;;                               nord-theme
;;                               solarized-theme
;;                               zenburn-theme
;;                               molokai-theme
;;                               darktooth-theme
;;                               gotham-theme
;;                               ample-theme
;;                               material-theme
;;                               leuven-theme
;;                               spacemacs-theme
;;                               gruvbox-theme
;;                               forest-blue-theme
;;                               flatland-theme
;;                               afternoon-theme
;;                               cyberpunk-theme
;;                               darkmine-theme
;;                               tao-theme
;;                               darkokai-theme
;;                               jazz-theme
;;                               suscolors-theme
;;                               omtose-phellack-theme
;;                               atom-one-dark-theme
;;                               nubox
;;                               color-theme-sanityinc-tomorrow
;;                               alect-themes
;;                               kaolin-themes
;;                               srcery-theme)
;;   "A list of themes to ensure are installed at launch.")

;; (defun appearance-packages-installed-p ()
;;   (loop for p in packages-appearance
;;         when (not (package-installed-p p)) do (return nil)
;;         finally (return t)))

;; (unless (appearance-packages-installed-p)
;;   ;; check for new packages (package versions)
;;   (message "%s" "Emacs is now refreshing its package themes...")
;;   (package-refresh-contents)
;;   (message "%s" " done.")
;;   ;; install the missing packages
;;   (dolist (p packages-appearance)
;;     (when (not (package-installed-p p))
;;       (package-install p))))

;; (provide 'packages-appearance)


;;; End setup-theme.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-theme)
