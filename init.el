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
                      flycheck-pos-tip
                      magit
                      r-autoyas
                      smex
                      scpaste
                      polymode
		      textmate
                      volatile-highlights
                      clean-aindent-mode
                      color-theme-sanityinc-tomorrow
                      yaml-mode
                      git-gutter-fringe+
                      rainbow-mode
                      fill-column-indicator
                      monokai-theme
                      ))

(let ((default-directory "~/.emacs.d/elpa/"))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path))

; install the missing packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(setq redisplay-dont-pause t)
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
	      scroll-down-aggressively 0.01)

; Load theme
(add-to-list 'load-path "~/.emacs.d/themes/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'monokai t)


;; ; Font
;; (set-face-attribute 'default nil :foundry "apple" :family "Monaco")
;; set all windows (emacs's "frame") to use font DejaVu Sans Mono
(set-frame-font "Menlo-12" t t)
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; no toolbar
(tool-bar-mode -1)

;; no scrollbars
(scroll-bar-mode -1)

;; mark fill column
(require 'fill-column-indicator)

;; Better looking terminal windows
;; Space after line numbers
(when (not(display-graphic-p)) (add-hook 'window-configuration-change-hook
					 (lambda ()
					   (setq linum-format "%4d "))))
;; Turn off menu bar
(when (not(display-graphic-p)) (menu-bar-mode -1))

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

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

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


(defun my-common-hook ()
  ;; my customizations for all of c-mode and related modes
  (when (display-graphic-p) (fci-mode nil))
  (linum-mode 1)
  (rainbow-mode 1) ;; Rainbow colors
  )

(add-hook 'prog-mode-hook 'my-common-hook)
(add-hook 'R-mode-hook 'my-common-hook)
(add-hook 'markdown-mode-hook 'my-common-hook)

; Load hook
(add-hook 'prog-mode-hook 'my-common-hook)
(add-hook 'ess-mode-hook 'my-common-hook)

;; Activate flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
'(flycheck-lintr-caching nil)

(add-hook 'css-mode-hook 'my-css-mode-hook)
(defun my-css-mode-hook ()
  (rainbow-mode 1))


;; Anything that writes to the buffer while the region is active will overwrite
;; it, including paste, but also simply typing something or hitting backspace
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
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
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

; Variables I set up from within emacs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(comint-move-point-for-output nil)
 '(comint-scroll-show-maximum-output nil)
 '(comint-scroll-to-bottom-on-input nil)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error nil)
 '(compilation-environment nil)
 '(compilation-read-command nil)
 '(compilation-scroll-output (quote first-error))
 '(compile-command "make")
 '(fci-rule-character-color "#272821")
 '(fci-rule-color "#272821")
 '(fci-rule-column 80)
 '(fill-column 80)
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(flycheck-lintr-caching nil)
 '(inhibit-startup-screen t)
 '(linum-delay t)
 '(linum-eager t)
 '(linum-format " %4d")
 '(magit-use-overlays nil))


;; Mark additions/deletions in a git repo, on the margin
(require 'git-gutter-fringe+)
(global-git-gutter+-mode t)
;; (set-face-foreground 'git-gutter+-modified "#e7c547")
;; (set-face-foreground 'git-gutter+-added    "#e7c547")
;; (set-face-foreground 'git-gutter+-deleted  "#e7c547")

;; Color of the fringe
(set-face-background 'fringe "#272821")

;; Adapted with one minor change from Felipe Salazar at
;; http://www.emacswiki.org/emacs/EmacsSpeaksStatistics

(setq ess-ask-for-ess-directory nil)
(setq ess-local-process-name "R")
(setq ess-history-directory "~/.R/")
(setq ansi-color-for-comint-mode 'filter)
;;(setq inferior-R-program-name "/usr/local/bin/R")
(setq ansi-color-for-comint-mode 'filter)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; Limit buffer size
(add-to-list 'comint-output-filter-functions 'comint-truncate-buffer)

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

; Do not replace _ with <-
(ess-toggle-underscore nil)

(defun my-ess-post-run-hook ()
  (local-set-key "\C-cw" 'ess-execute-screen-options))
(add-hook 'ess-post-run-hook 'my-ess-post-run-hook)

; Save/load history of minibuffer
(savehist-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (set-face-attribute 'ac-candidate-face nil   :background "#00222c" :foreground "light gray")
;; (set-face-attribute 'ac-selection-face nil   :background "SteelBlue4" :foreground "white")
;; (set-face-attribute 'popup-tip-face    nil   :background "#003A4E" :foreground "light gray")

(add-hook 'ess-mode (lambda () (add-to-list 'ac-sources 'ac-source-R)))

(defvar sanityinc/fci-mode-suppressed nil)
(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
  (when fci-mode
    (turn-off-fci-mode)))
(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))
