;;; init.el --- Default init file

;;; Commentary:
;;
;; Lisp for initalizing my Emacs.
;;
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

;;; Code:
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; set up use-package macros
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; setup load path for unpackaged lisp code
(let ((default-directory (concat user-emacs-directory "elisp/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

;; This exists solely to get rid of the annoying "assignment to free variable"
;; warning.
(eval-when-compile
  (defvar ispell-program-name)
  (defvar mouse-sel-mode))

;;
;; Set up OS X specific settings
;;
(when (eq system-type 'darwin)
  ;; setup some program specific paths
  (setq ispell-program-name "/usr/local/bin/ispell")
  (setq latex-run-command "/Library/TeX/texbin/latex")

  ;; Disable Apple's Full-Screen mode
  (setq ns-use-native-fullscreen nil))

;;
;; running in a terminal, so setup mouse
;;
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (defun track-mouse (e))
  (setq mouse-sel-mode t))

;; load some custom functions
(load (concat user-emacs-directory "funcs.el"))

;; separate custom-file from init.el
(setq custom-file (concat user-emacs-directory "custom-settings.el"))

;; custom code that isn't packaged
(when (require 'so-long nil :noerror)
  (so-long-enable))

;;
;; custom backup paths
;;
(let ((user-backup-directory (concat user-emacs-directory "backups/")))
  (setq backup-directory-alist `((".*" . ,user-backup-directory)))
  (setq auto-save-file-name-transforms `((".*" ,user-backup-directory t)))
  (setq auto-save-list-file-prefix user-backup-directory))

;;
;; some custom global keymappings
;;
(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)

;; Some convenience things
(global-set-key [f2] 'visit-ansi-term)
(global-set-key [f8] 'toggle-fullscreen)

(global-set-key "\M-g" 'goto-line)

(global-set-key (kbd "<M-S-up>") 'switch-to-prev-buffer)
(global-set-key (kbd "<M-S-down>") 'switch-to-next-buffer)

(global-set-key (kbd "<C-S-up>") 'enlarge-window)
(global-set-key (kbd "<C-S-down>") 'shrink-window)
(global-set-key (kbd "<C-S-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-S-left>") 'shrink-window-horizontally)

;; re-enable some functions
;;   C-x n d ... narrow to def
;;   C-x n n ... narrow to region
;;   C-x n p ... narrow to page
;;   C-x n w ... widen back
;; enable some features
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(put 'scroll-left 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; set up default window frame stuff
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 100))

;; emacs 27 deprecates cl and some packages still usage it. This removes
;; an annoying warning
(if (version<= "27" emacs-version)
    (setq byte-compile-warnings '(cl-functions)))

;;
;; Package settings
;;
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package ggtags
  :ensure t
  :demand t)

;;
;; Helm and Projectile
;;
(use-package helm
  :ensure t
  :demand t

  :bind
  ("M-x" . helm-M-x)
  ("M-y" . helm-show-kill-ring)
  ("C-x h" . helm-mini)
  ("C-c C-r" . helm-recentf)

  (:map helm-map
        ("<tab>" . helm-execute-persistent-action)
        ("C-i" . helm-execute-persistent-action)
        ("C-z" . helm-select-action))

  :bind-keymap
  ("C-c h" . helm-command-map)

  :config
  (use-package helm-config
    :demand t
    :bind
    (:map helm-command-map
          ("a" . helm-ag)
          ("o" . helm-occur))
    )

  (use-package helm-ag
    :ensure t
    :demand t)

  ;; This exists solely to get rid of the annoying "assignment to free variable"
  ;; warning.
  (eval-when-compile
    (defvar helm-quick-update)
    (defvar helm-split-window-inside-p)
    (defvar helm-buffers-fuzzy-matching)
    (defvar helm-ff-search-library-in-sexp)
    (defvar helm-ff-file-name-history-use-recentf))

  (setq helm-quick-update t)
  (setq helm-split-window-inside-p t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-move-to-line-cycle-in-source 0)
  (setq helm-ff-search-library-in-sexp t)
  (setq helm-scroll-amount 8)
  (setq helm-ff-file-name-history-use-recentf t)

  (helm-mode 1))

(use-package projectile
  :ensure t
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)

  :init
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'helm-projectile)
  (setq projectile-find-dir-includes-top-level t)
  (setq projectile-enable-idle-timer t)
  (setq projectile-globally-ignored-file-suffixes '(".min.js"))

  :config
  (use-package helm-projectile
    :ensure t
    :config
    (helm-projectile-on))

  (use-package projectile-rails
    :ensure t)

  (projectile-mode 1)
  (projectile-rails-global-mode))

;;
;; Mode settings
;;
(use-package alchemist
  :ensure t
  )

(use-package bibtex
  :config
  (setq bibtex-entry-format '(opts-or-alts realign last-comma delimiters page-dashes))
  (setq bibtex-autokey-year-length 4)
  (setq bibtex-autokey-year-title-separator "")
  (setq bibtex-autokey-titleword-length nil))

(use-package cc-mode
  :after (company)

  :config
  (use-package company-c-headers
    :ensure t
    :config
    (add-to-list 'company-backends 'company-c-headers))

  (add-hook 'write-contents-functions 'untabify-before-save))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package compile
  :config
  (setq truncate-lines 1)
  (setq truncate-partial-width-windows 1))

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package fill-column-indicator
  :ensure t
  :init
  (setq fci-handle-truncate-lines nil)
  (setq fci-rule-width 1)
  (setq fci-rule-color "darkblue")
  (setq fci-rule-width 2)
  (setq-default fci-rule-column 80))

(use-package ruby-mode
  :ensure t
  :mode
  "\\.rb\\'"
  "Rakefile\\'"
  "Gemfile\\'"
  "Vagrantfile\\'"

  :interpreter "ruby"

  :config
  (use-package chruby
    :ensure t)

  (use-package company-inf-ruby
    :ensure t
    :config
    (add-to-list 'company-backends 'company-inf-ruby))

  (use-package robe
    :ensure t
    :hook
    (ruby-mode . robe-mode)
    :config
    (add-to-list 'company-backends 'company-robe))

  (use-package rspec-mode
    :ensure t
    :hook
    ruby-mode)

  (use-package rubocop
    :ensure t
    :hook
    (ruby-mode . rubocop-mode))

  (use-package yard-mode
    :ensure t
    :hook
    ruby-mode)

  (setq ruby-deep-indent-paren nil)
  (setq fill-column 80)
  (setq fci-rule-column 80)

  (defun custom-ruby-mode-hook ()
    (chruby-use-corresponding))

  (add-hook 'ruby-mode-hook 'custom-ruby-mode-hook))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package hl-line
  :config
  (set-face-background hl-line-face "color-238")
  (global-hl-line-mode 1))

(use-package hilit-chg
  :config
  (highlight-changes-mode t))

(use-package js2-mode
  :ensure t
  :mode
  ("\\.js\\'" . js2-mode)
  :config
  (setq js-indent-level 2))

(use-package json-mode
  :ensure t
  :config
  (make-local-variable 'js-indent-level)
  (setq js-indent-level 2))

;; emacs 26+ introduced display-line-numbers, which has better aethetic results
;; (e.g., line numbers on blank lines)
(if (version< emacs-version "26")
    (use-package linum
      :config
      (setq linum-format "%d ")
      (global-linum-mode t))
    (use-package display-line-numbers
      :config
      (global-display-line-numbers-mode)))

(use-package magit
  :ensure t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-magit-file-mode 1))

(use-package markdown-mode
  :ensure t)

(use-package origami
  :ensure t)

(use-package paren
  :config
  (setq show-paren-style 'mixed)
  (show-paren-mode 1))

(use-package prog-mode
  :config
  (defun custom-prog-mode-hook()
    (ggtags-mode 1)
    (fci-mode 1))

  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'prog-mode-hook 'custom-prog-mode-hook))

(use-package python
  :mode
  ("\\.py\\'" . python-mode)

  :after (company)

  :config
  (use-package anaconda-mode
    :ensure t)

  (use-package company-anaconda
    :ensure t
    :config
    (add-to-list 'company-backends 'company-anaconda))

  (setq fill-column 80)
  (setq fci-rule-column 132)
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)

  (defun custom-python-hook ()
    (flycheck-mode 0)
    (anaconda-mode 1)
    (anaconda-eldoc-mode 1)
    (electric-indent-local-mode 0))

  (add-hook 'python-mode-hook 'custom-python-hook))

(use-package recentf
  :config
  (setq recentf-max-menu-items 25)
  (recentf-mode 1))

(use-package saveplace
  :ensure t
  :config
  (setq save-place-file (concat user-emacs-directory "places"))
  (save-place-mode 1))

(use-package scss-mode
  :ensure t)

(use-package semantic
  :ensure t
  :config
  (global-semantic-highlight-func-mode t)
  (global-semantic-highlight-edits-mode t)
  (global-semantic-stickyfunc-mode -1))

(use-package shell
  :config
  (setq fci-rule-column 132)

  ;; This exists solely to get rid of the annoying "assignment to free variable"
  ;; warning.
  (eval-when-compile
    (defvar sh-indentation)
    (defvar sh-basic-offset))

  ;; sh-indentation is obsoleted since 26.1
  (if (version< emacs-version "26.1")
      (setq sh-indentation 2)
    (setq sh-basic-offset 2)))

(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode))

(use-package text-mode
  :config
  (setq fill-column 80)
  (setq indent-line-function (quote insert-tab))
  (setq indent-tabs-mode nil)
  (setq tab-width 2))

(use-package tramp
  :ensure t
  :config
  (setq tramp-default-method "sshx"))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package yaml-mode
  :ensure t
  :config
  (setq yaml-indent-offset 2))

(use-package web-mode
  :mode
  ("\\.phtml\\'" . web-mode)
  ("\\.tpl\\.php\\'" . web-mode)
  ("\\.[agj]sp\\'" . web-mode)
  ("\\.as[cp]x\\'" . web-mode)
  ("\\.erb\\'" . web-mode)
  ("\\.mustache\\'" . web-mode)
  ("\\.djhtml\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  ("\\.css?\\'" . web-mode)

  :config
  ;; This exists solely to get rid of the annoying "assignment to free variable"
  ;; warning.
  (eval-when-compile
    (defvar web-mode-markup-indent-offset)
    (defvar web-mode-code-indent-offset 2))

  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package whitespace
  :config
  (setq whitespace-style '(face trailing tabs lines lines-tail))
  (global-whitespace-mode t))

(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package monokai-pro-theme
  :ensure t
  :config
  (set-face-attribute 'default nil
                      :family "Cascadia Code"
                      :weight 'normal
                      :width 'normal
                      :height 160))

(setq explicit-shell-file-name "/bin/bash")
(setq frame-title-format
      '(buffer-file-name "%f" (dired-directory dired-directory "%b")))
(setq inhibit-startup-screen t)
(setq locale-coding-system 'utf-8)

(setq standard-indent 2)
(setq tags-revert-without-query 1)
(setq transient-mark-mode t)
(setq truncate-partial-width-windows nil)
(setq visible-bell t)

(setq-default buffer-file-coding-system 'undecided-unix)
(setq-default fill-column 80)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)
(setq-default large-file-warning-threshold nil)
(setq-default savehist-mode t)
(setq-default truncate-lines t)

;; coding system
(prefer-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; various modes I like
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)
(tool-bar-mode -1)
(global-font-lock-mode 1)

(load custom-file)

(load-theme 'monokai-pro)

;;; init.el ends here
