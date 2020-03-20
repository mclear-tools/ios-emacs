;;; Commentary:
;; Base init file to load config. Use "outshine-cycle-buffer" (<Tab> and <S-Tab>
;; in org style) to navigate through sections, and "imenu" to locate individual
;; use-package definition.

;;; Startup
;;;; Check Errors
;; Produce backtraces when errors occur
(setq debug-on-error nil)

;;;; Clean View
;; Disable start-up screen
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(menu-bar-mode -1)

;; Quick start scratch buffer
(setq initial-major-mode 'fundamental-mode)

;; Bury the scratch buffer, don't kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))

;;; Use-Package Settings
;; I tell use-package to always defer loading packages unless explicitly told
;; otherwise. This speeds up initialization significantly as many packages are
;; only loaded later when they are explicitly used. But it can also cause
;; problems:
;; https://github.com/jwiegley/use-package#loading-packages-in-sequence. I also
;; put a lot of loading of packages off until after some number of seconds of idle. The
;; latter means package loading stays out of my way if I'm doing, e.g., a quick
;; restart-and-check of something in emacs.

(setq use-package-always-defer t
      use-package-verbose t
      use-package-minimum-reported-time 0.01
      use-package-enable-imenu-support t
      use-package-always-ensure t)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ;; ("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ;;  https://github.com/emacs-china/emacswiki-elpa
                         ("emacswiki" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/")
                         ))
(require 'package)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)


;;;; Auto-compile
;; Automatically byte-recompile changed elisp libraries
(use-package auto-compile
  :ensure t
  :defer 1
  :config
  (setq auto-compile-display-buffer nil)
  (setq auto-compile-mode-line-counter t)
  (setq auto-compile-update-autoloads t)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;; Personal Information
;; Give emacs some personal info
(setq user-full-name "Colin McLear"
      user-mail-address "mclear@fastmail.com")

;;;; Directory Variables
;;  We're going to define a number of directories that are used throughout this
;;  configuration to store different types of files.

(defconst cpm-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

(defconst cpm-local-dir (concat cpm-emacs-dir ".local/")
    "Root directory for local Emacs files. Use this as permanent
  storage for files that are safe to share across systems (if
  this config is symlinked across several computers).")

(defconst cpm-temp-dir (concat cpm-local-dir "temp/")
    "Directory for non-essential file storage. Used by
  `cpm-etc-dir' and `cpm-cache-dir'.")

(defconst cpm-etc-dir (concat cpm-temp-dir "etc/")
    "Directory for non-volatile storage. These are not deleted or
  tampered with by emacs functions. Use this for dependencies
  like servers or config files that are stable (i.e. it should be
  unlikely that you need to delete them if something goes
  wrong).")

(defconst cpm-cache-dir (concat cpm-temp-dir "cache/")
    "Directory for volatile storage. Use this for transient files
  that are generated on the fly like caches and temporary files.
  Anything that may need to be cleared if there are problems.")

(defconst cpm-elisp-dir (concat cpm-local-dir "elisp/")
  "Where personal elisp packages and scripts are stored.")

(defconst cpm-setup-dir (concat cpm-emacs-dir "setup-config/")
  "Where the setup-init files are stored.")

;;;; Load Path
;; We're going to set the load path ourselves so that we don't have to call
;; `package-initialize` at runtime and incur a large performance hit. This
;; load-path will actually be faster than the one created by
;; `package-initialize` because it appends the elpa packages to the end of the
;; load path. Otherwise any time a builtin package was required it would have to
;; search all of third party paths first.
(setq load-path (append load-path (directory-files package-user-dir t "^[^.]" t)))
(push cpm-setup-dir load-path)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(setq shell-file-name "/bin/zsh")

;;; Load Modules
;; Load all the setup modules

;;;; Core Modules
;; These are the "can't live without" modules
(require 'setup-libraries)
(require 'setup-keybindings)
(require 'setup-evil)
(require 'setup-settings)
(require 'setup-dired)
(require 'setup-ivy)
(require 'setup-helm)

;;;; Other Modules
(require 'setup-ui)
(require 'setup-functions-macros)
(require 'setup-modeline)
(require 'setup-theme)
(require 'setup-osx)
(require 'setup-windows)
(require 'setup-navigation)
(require 'setup-search)
(require 'setup-vc)
;; (require 'setup-shell)
;; (require 'setup-org)
(require 'setup-writing)
(require 'setup-projects)
;; (require 'setup-programming)
;; (require 'setup-pdf)
;; (require 'setup-calendars)
;; (require 'setup-completion)
;; (require 'setup-dashboard)
;; (require 'setup-posframe)
;; (require 'setup-testing)

;;; Config Helper Functions

;;;; Config Navigation
;; Function to navigate config files
(defun cpm/find-files-setup-config-directory ()
  "use counsel to find setup files"
  (interactive)
  (counsel-find-file cpm-setup-dir))
;; (helm-find-files-1 cpm-setup-dir))

;; Function to search config files
(defun cpm/search-setup-config-files ()
  "use counsel rg to search all config files"
  (interactive)
  (counsel-rg nil cpm-setup-dir))
;; (helm-do-ag cpm-setup-dir))

;; Load init file
(defun cpm/load-init-file ()
  "load the base init file"
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;;;; Byte Compile Config Files
;; https://emacsredux.com/blog/2013/06/25/boost-performance-by-leveraging-byte-compilation/
(defun cpm/byte-compile-dotemacs ()
  "Byte compile all files in the .emacs.d base directory"
  (interactive)
  (shell-command-to-string "trash ~/.emacs.d/*.elc && trash ~/.emacs.d/setup-config/*.elc")
  (byte-recompile-directory user-emacs-directory 0 t))

(defun cpm/delete-byte-compiled-files ()
  "Delete byte-compiled files"
  (interactive)
  (shell-command-to-string "trash ~/.emacs.d/*.elc && trash ~/.emacs.d/setup-config/*.elc"))


;; Startup time
(message (format "Emacs ready in %.2f seconds with %d garbage collections."
                 (float-time
                  (time-subtract after-init-time before-init-time)) gcs-done))
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
