(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not package-archive-contents) (package-refresh-contents))

(defvar my-packages '(
                      auto-complete
                      ac-cider
                      better-defaults
                      cider
                      exec-path-from-shell
                      find-file-in-project
                      idle-highlight-mode
                      ido-ubiquitous
                      flx-ido
                      markdown-mode
                      flycheck
                      magit
                      r-autoyas
                      smex
                      scpaste
                      polymode
		      textmate
                      volatile-highlights
                      clean-aindent-mode
                      color-theme-sanityinc-tomorrow
                      ))

(let ((default-directory "~/.emacs.d/elpa/"))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

; Font
(set-face-attribute 'default nil :foundry "apple" :family "Monaco")

(setq redisplay-dont-pause t)
(setq redisplay-dont-pause t
  scroll-margin 1
  scroll-step 1
  scroll-conservatively 10000
  scroll-preserve-screen-position 1)
(setq-default scroll-up-aggressively 0.01 scroll-down-aggressively 0.01)

; Load theme
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'tomorrow-night t)

(require 'color-theme-sanityinc-tomorrow)
(require 'sanityinc-tomorrow-day-theme)
(color-theme-sanityinc-tomorrow-day)


; Desactivate alarm
(setq ring-bell-function 'ignore)

;; Emacs will not automatically add new lines (stop scrolling at end of file)
(setq next-line-add-newlines nil)

; Prevent functions to access the clipboard
(setq x-select-enable-clipboard nil)

; Yes or No in one character
(defalias 'yes-or-no-p 'y-or-n-p)

; Get rid of temporary files
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;smek provides the history on top of M-x
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
      (exec-path-from-shell-initialize))

(textmate-mode)

;; Highlights volatile actitions such as paste
(require 'volatile-highlights)
(volatile-highlights-mode t)

; magit
(require 'magit)

; Ido to navigate the filesystem
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-use-virtual-buffers t
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-max-prospects 10
      ido-default-file-method 'selected-window
      ido-auto-merge-work-directories-length -1)

(ido-mode +1)
(ido-ubiquitous-mode +1)
;;; smarter fuzzy matching for ido
(flx-ido-mode +1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)


; Line numbers
(global-linum-mode 1)

;; Anything that writes to the buffer while the region is active will overwrite it, including paste, but also simply typing something or hitting backspace
(delete-selection-mode 1)

; Autocomplete
(require 'auto-complete-config)
(ac-config-default)

;; Electric pair, indentation, layout
(electric-indent-mode 1)
(electric-pair-mode 1)
(electric-layout-mode 1)

; spell checker
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;; Allows the right alt key to work as alt
(setq ns-right-alternate-modifier nil)

; Some keybindings
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key [C-tab] 'other-window) ;Change windows

; Markdown mode
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown$" . markdown-mode))
(add-hook 'markdown-mode-hook
          (lambda ()
            (visual-line-mode t)
            (flyspell-mode t)))
(setq markdown-command "pandoc --smart -f markdown -t html")
; (setq markdown-css-path (expand-file-name "markdown.css" abedra/vendor-dir))

; Ruby
(add-hook 'ruby-mode-hook
          (lambda ()
            (autopair-mode)))

(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

; YAML mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;polymode
(require 'poly-R)
(require 'poly-markdown)

;;; MARKDOWN
(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

;;; R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))


; (require 'r-autoyas)
; (add hook 'ess-mode-hook 'r-autoyas-ess-activate)

; ;;
; ;; Load autocomplete
; ;;
; (require 'auto-complete)
; (require 'auto-complete-config)
; (global-auto-complete-mode t)
; (add-to-list 'ac-dictionary-directories (expand-file-name "auto-complete" dotfiles-dir))
; (setq ac-modes (append ac-modes '(org-mode)))
; (ac-config-default)
; (define-key ac-complete-mode-map [tab] 'ac-expand)
; (setq ac-auto-start 4)
; (ac-flyspell-workaround)
; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
; (define-key ac-completing-map (kbd "C-c h") 'ac-quick-help)



;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics

(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
;;(setq inferior-R-program-name "/usr/local/bin/R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)
(defun my-ess-start-R ()
  (interactive)
  (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
      (progn
        (delete-other-windows)
        (setq w1 (selected-window))
        (setq w1name (buffer-name))
        (setq w2 (split-window w1 nil t))
        (R)
        (set-window-buffer w2 "*R*")
        (set-window-buffer w1 w1name))))
(defun my-ess-eval ()
  (interactive)
  (my-ess-start-R)
  (if (and transient-mark-mode mark-active)
      (call-interactively 'ess-eval-region)
    (call-interactively 'ess-eval-line-and-step)))
(add-hook 'ess-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))
(add-hook 'inferior-ess-mode-hook
          '(lambda()
             (local-set-key [up] 'comint-previous-input)
             (local-set-key [down] 'comint-next-input)))
(add-hook 'Rnw-mode-hook
          '(lambda()
             (local-set-key [(shift return)] 'my-ess-eval)))
(require 'ess-site)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "645599a2aab022fd7677124515a3104a60ba64d2cafdd77a6e7703f8ae97250c" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "7f1263c969f04a8e58f9441f4ba4d7fb1302243355cb9faecb55aec878a06ee9" default)))
 '(inhibit-startup-screen t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(defvar lawlist-movement-syntax-table
  (let ((st (make-syntax-table)))
    ;; ` default = punctuation
    ;; ' default = punctuation
    ;; , default = punctuation
    ;; ; default = punctuation
    (modify-syntax-entry ?{ "." st)  ;; { = punctuation
    (modify-syntax-entry ?} "." st)  ;; } = punctuation
    (modify-syntax-entry ?\" "." st) ;; " = punctuation
    (modify-syntax-entry ?\\ "_" st) ;; \ = symbol
    (modify-syntax-entry ?\$ "_" st) ;; $ = symbol
    (modify-syntax-entry ?\% "_" st) ;; % = symbol
    st)
  "Syntax table used while executing custom movement functions.")

(defun delete-word-or-whitespace ()
"http://stackoverflow.com/a/20456861/2112489"
(interactive)
  (with-syntax-table lawlist-movement-syntax-table
    (let* (
        (transient-mark-mode t)
        (word-regexp "\\sw")
        (punctuation-regexp "\\s.")
        (symbol-regexp "\\s_\\|\\s(\\|\\s)"))
      (cond
        ;; right of cursor = word or punctuation or symbol
        ((or
            (save-excursion (< 0 (skip-syntax-forward "w")))
            (save-excursion (< 0 (skip-syntax-forward ".")))
            (save-excursion (< 0 (skip-syntax-forward "_()"))))
          (cond
            ;; right of cursor = word
            ((save-excursion (< 0 (skip-syntax-forward "w")))
              (skip-syntax-forward "w")
              (let ((end (point)))
                (while (looking-back word-regexp)
                  (backward-char))
                (let ((beg (point)))
                  (delete-region beg end))))
            ;; right of cursor = punctuation
            ((save-excursion (< 0 (skip-syntax-forward ".")))
              (skip-syntax-forward ".")
              (let ((end (point)))
                (while (looking-back punctuation-regexp)
                  (backward-char))
                (let ((beg (point)))
                  (delete-region beg end))))
            ;; right of cursor = symbol
            ((save-excursion (< 0 (skip-syntax-forward "_()")))
              (skip-syntax-forward "_()")
              (let ((end (point)))
                (while (looking-back symbol-regexp)
                  (backward-char))
                (let ((beg (point)))
                  (delete-region beg end)))))
          (cond
            ;; right of cursor = whitespace
            ;; left of cursor = not word / not symbol / not punctuation = whitespace or bol
            ((and
                (save-excursion (< 0 (skip-chars-forward " \t")))
                (not (save-excursion (> 0 (skip-syntax-backward "w"))))
                (not (save-excursion (> 0 (skip-syntax-backward "."))))
                (not (save-excursion (> 0 (skip-syntax-backward "_()")))))
              (let ((beg (point)))
                (skip-chars-forward " \t")
                (let ((end (point)))
                  (delete-region beg end))))
            ;; right of cursor = whitespace
            ;; left of cursor = word or symbol or punctuation
            ((and
                (save-excursion (< 0 (skip-chars-forward " \t")))
                (or
                  (save-excursion (> 0 (skip-syntax-backward "w")))
                  (save-excursion (> 0 (skip-syntax-backward ".")))
                  (save-excursion (> 0 (skip-syntax-backward "_()")))))
              (fixup-whitespace))))
        ;; right of cursor = whitespace
        ;; left of cursor = bol | left of cursor = whitespace | right of cursor = whitespace + eol
        ((and 
            (save-excursion (< 0 (skip-chars-forward " \t")))
            (or
              (bolp)
              (save-excursion (> 0 (skip-chars-backward " \t")))
              (save-excursion (< 0 (skip-chars-forward " \t")) (eolp))))
          (let ((beg (point)))
            (skip-chars-forward " \t")
              (let ((end (point)))
                (delete-region beg end))))
        ;; right of cursor = whitespace or eol
        ;; left of cursor = word or symbol or punctuation
        ;; not bol + word or symbol or punctuation
        ;; not bol + whitespace + word or symbol or punctuation
        ((and 
            (or (save-excursion (< 0 (skip-chars-forward " \t"))) (eolp))
            (or
              (save-excursion (> 0 (skip-syntax-backward "w")))
              (save-excursion (> 0 (skip-syntax-backward ".")))
              (save-excursion (> 0 (skip-syntax-backward "_()"))))
            (not (save-excursion (> 0 (skip-syntax-backward "w")) (bolp)))
            (not (save-excursion (> 0 (skip-syntax-backward ".")) (bolp)))
            (not (save-excursion (> 0 (skip-syntax-backward "_()")) (bolp)))
            (not (save-excursion (and (> 0 (skip-syntax-backward "w")) (> 0 (skip-chars-backward " \t")) (bolp))))
            (not (save-excursion (and (> 0 (skip-syntax-backward ".")) (> 0 (skip-chars-backward " \t")) (bolp))))
            (not (save-excursion (and (> 0 (skip-syntax-backward "_()")) (> 0 (skip-chars-backward " \t")) (bolp)))))
          (let ((end (point)))
            (set-mark end)
            (cond
              ((save-excursion (> 0 (skip-syntax-backward "w")))
                (while (looking-back word-regexp)
                  (backward-char)))
              ((save-excursion (> 0 (skip-syntax-backward ".")))
                (while (looking-back punctuation-regexp)
                  (backward-char)))
              ((save-excursion (> 0 (skip-syntax-backward "_()")))
                (while (looking-back symbol-regexp)
                  (backward-char))))
            (let ((beg (point)))
              (cond
                ((save-excursion (> 0 (skip-chars-backward " \t")))
                  (skip-chars-backward " \t")
                  (setq beg (point))))
              (delete-region beg end)
              (skip-chars-forward " \t"))))
        ;; not bol = eol
        ;; left of cursor = bol + word or symbol or punctuation | bol + whitespace + word or symbol or punctuation
        ((and
            (not (and (bolp) (eolp)))
            (or
              (save-excursion (> 0 (skip-syntax-backward "w")) (bolp))
              (save-excursion (> 0 (skip-syntax-backward ".")) (bolp))
              (save-excursion (> 0 (skip-syntax-backward "_()")) (bolp))
              (save-excursion (and (> 0 (skip-syntax-backward "w")) (> 0 (skip-chars-backward " \t")) (bolp)))
              (save-excursion (and (> 0 (skip-syntax-backward ".")) (> 0 (skip-chars-backward " \t")) (bolp)))
              (save-excursion (and (> 0 (skip-syntax-backward "_()")) (> 0 (skip-chars-backward " \t")) (bolp)))))
          (let ((start (point)))
            (skip-chars-forward " \t")
            (let ((end (point)))
              (set-mark end)
              (beginning-of-line)
              (let ((beg (point)))
                (delete-region beg end)))))) )))

(global-set-key (kbd "M-DEL") 'delete-word-or-whitespace)
