;; Writing

;;; Spelling
(use-package ispell
  :commands (ispell-word ispell-region ispell-buffer)
:config
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")))
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    ;;(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))
  ;; (when (executable-find "hunspell")
  ;;   (setq-default ispell-program-name "hunspell")
  ;;   (setq ispell-extra-args   '("-d en_US"))
  ;;   (setq ispell-really-hunspell t)))

  ;; Save a new word to personal dictionary without asking
  ;; (setq ispell-silently-savep nil)

  ;; (setq-default ispell-program-name "/usr/local/bin/aspell")
  ;; (setq ispell-extra-args
  ;;     (list "--sug-mode=fast" ;; ultra|fast|normal|bad-spellers
  ;;           "--lang=en_US"
  ;;           "--ignore=3")))

(use-package flyspell
  :config
  (setq flyspell-abbrev-p t
        flyspell-use-global-abbrev-table-p t
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :hook (;;(markdown-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

(use-package flyspell-correct-ivy
  :general
  (:states '(normal insert emacs) :keymaps 'flyspell-mode-map
   "C-;" 'flyspell-auto-correct-previous-word
   "C-:" 'flyspell-correct-word-generic))

(with-eval-after-load 'hydra
  (defhydra hydra-spelling (:color blue)
  "
  ^
  ^Spelling^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^───────
  _q_ quit            _<_ previous        _c_ correction
  ^^                  _>_ next            _d_ dictionary
  ^^                  _f_ check           _m_ mode
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("<" flyspell-correct-previous :color pink)
  (">" flyspell-correct-next :color pink)
  ("c" ispell)
  ("d" ispell-change-dictionary)
  ("f" flyspell-buffer :color pink)
  ("m" flyspell-mode)))

;;; Abbrev
(use-package abbrev
  :ensure nil
  :defer 2
  :config
  ;; (add-hook 'text-mode-hook #'abbrev-mode)
  (setq abbrev-file-name (concat cpm-local-dir "abbrev/.abbrev_defs")
        save-abbrevs 'nil)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :config
  (setq markdown-command
        (concat
         "/usr/local/bin/pandoc"
         " --from=markdown --to=html"
         " --standalone --mathjax --highlight-style=pygments"
         " --css=/Users/roambot/.pandoc/pandoc.css"
         " --quiet"
         " --number-sections"
         " --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua"
         " --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua"
         " --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml"
         " --metadata=reference-section-title:'References & Further Reading'"
         " --filter pandoc-citeproc"
         ;; " --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib"
         ))

  (setq markdown-enable-math nil
        markdown-enable-wiki-links t
        markdown-nested-imenu-heading-index t
        markdown-open-command "~/bin/mark.sh"
        markdown-footnote-location 'immediately
        markdown-unordered-list-item-prefix "-   "
        markdown-use-pandoc-style-yaml-metadata t)
  ;; markdown hooks
  (add-hook 'markdown-mode-hook
            '(lambda ()
               (turn-on-flyspell) (auto-fill-mode) (hl-todo-mode))))

;; macro: delete backslashes in paragraph to cleanup markdown conversion
(fset 'cpm/md-delete-backslash
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\361\361f\\x" 0 "%d")) arg)))

;;; Markdown TOC
(use-package markdown-toc
  :ensure t
  :after markdown
  :hook (markdown-mode . markdown-toc))

;;; Pandoc
(use-package pandoc-mode
  :commands (cpm/pandoc-convert-to-pdf run-pandoc pandoc-convert-to-pdf)
  :config
  (setq pandoc-use-async t)
  ;; stop pandoc from just hanging forever and not completing conversion
  ;; see https://github.com/joostkremers/pandoc-mode/issues/44
  (setq pandoc-process-connection-type nil)
  (progn
    (defun run-pandoc ()
      "Start pandoc for the buffer and open the menu"
      (interactive)
      (pandoc-mode)
      (pandoc-main-hydra/body))
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

  (defun cpm/pandoc-convert-to-pdf ()
   (interactive)
   (cond
   ((eq major-mode 'org-mode)
    (call-interactively 'org-pandoc-export-to-latex-pdf-and-open))
   (t
    (call-interactively 'pandoc-convert-to-pdf) (cpm/pandoc-pdf-open) (evil-window-prev 1))))

  (defun cpm/pandoc-command-line-convert-to-pdf ()
   "convert to pdf"
   (interactive)
   (evil-ex "!pandoc -s -N -V mainfont=Optima --pdf-engine=xelatex --bibliography=~/Dropbox/Work/bibfile.bib --template=~/.pandoc/pandoc-templates/default.latex -o '%.pdf' '%'"))

  (defun cpm/pandoc-pdf-open ()
   "Open created PDF file"
   (interactive)
   (find-file-other-window (concat (file-name-sans-extension buffer-file-name) ".pdf"))))
  :init
  (progn
    (setq pandoc-data-dir (concat cpm-local-dir "pandoc-mode/"))
    ;; help pandoc find xelatex
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))))

;;; Writeroom
(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-width 90)
  (setq writeroom-mode-line t)
  (setq writeroom-bottom-divider-width 0))

;; Set up a distraction free space
(defun distraction-free ()
  "distraction free writing"
  (interactive)
  (git-gutter-mode 0)
  (linum-mode 0)
  (centered-cursor-mode)
  (writeroom-mode)
  )


;;; Palimpsest (make archive)
(use-package palimpsest
  :diminish palimpsest-mode
  :hook ((markdown-mode org-mode) . palimpsest-mode)
  :config
  (setq palimpsest-trash-file-suffix ".archive"))





;;; Notes / Deft
(use-package deft
  :ensure t
  :commands (deft deft-open-file-other-window cpm/notebook deft-new-file-named)
  :general
  (:keymaps 'deft-mode-map :states '(normal motion)
   "o" 'cpm/deft-open
   "p" 'cpm/deft-open-preview
   "q" 'kill-this-buffer)
  (:keymaps 'deft-mode-map :states '(insert)
   "C-j" 'evil-next-line
   "C-k" 'evil-previous-line
   "C-o" 'cpm/deft-open
   "C-p" 'cpm/deft-open-preview)
  :config
  (with-eval-after-load 'evil
    (add-to-list 'evil-insert-state-modes 'deft-mode))
  ;; basic settings for use with zettel
  (setq deft-directory (concat (getenv "HOME") "/Dropbox/work/projects/notebook/org")
        deft-recursive t
        deft-use-filename-as-title t
        deft-separator " "
        deft-extensions '("org" "txt" "md")
        deft-default-extension "org")
  ;; file renaming rules
  (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  (setq deft-strip-summary-regexp
        (concat "\\("
                "[\n\t]" ;; blank
                "\\|^#\\+[a-zA-Z_]+:.*$" ;;org-mode metadata
                ;;yaml metadata
                "\\|^\\-\\{3\\}$"
                "\\|^[a-zA-Z_]+:.*$"
                "\\|@[a-zA-Z_].*$"
                ;; line beginning with markdown links
                "\\|^\\[.*$"
                "\\|^# .*$" ;; md titles
                "\\)"))

  ;;function to run deft in specified directory
  (defun any-deft (dir)
    "Run deft in directory DIR"
    (setq deft-directory dir)
    (switch-to-buffer "*Deft*")
    (kill-this-buffer)
    (deft))
  (defun cpm/notebook ()
    "Goto main notes with deft"
    (interactive)
    (any-deft "~/Dropbox/Work/projects/notebook/org")
    (kill-this-buffer)
    (any-deft "~/Dropbox/Work/projects/notebook/org"))
  (defun cpm/deft-open ()
    (interactive)
    (deft-open-file-other-window t))
  (defun cpm/deft-open-preview ()
    (interactive)
    (deft-open-file-other-window)))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'setup-writing)
