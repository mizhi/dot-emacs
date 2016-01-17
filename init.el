;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(unless (boundp 'user-emacs-directory)
  (setq user-emacs-directory
        (concat (getenv "HOME") "/.emacs.d/")))

;; send my backups to a subdirectory under my emacs directory
(let ((user-backup-directory (concat user-emacs-directory "backups/")))
  (setq backup-directory-alist `((".*" . ,user-backup-directory)))
  (setq auto-save-file-name-transforms `((".*" ,user-backup-directory t))))

;; setup load path
(let ((default-directory (concat user-emacs-directory "elisp/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(defun load-init-el (filename)
  (load (concat user-emacs-directory filename)))

(when (>= emacs-major-version 24)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/") t)
  (load-init-el "packages.el"))
  ;; (when (< 25 emacs-major-version)
  ;;   (load-init-el "packages.el")))


(load-init-el "funcs.el")

;; platform specific configuration
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin")

  (when window-system
    (server-start))

  (setq-default ispell-program-name
		(first-existing-file '("/opt/local/bin/ispell" "/usr/local/bin/ispell")))
  (setq latex-run-command "/usr/texbin/latex"
	;; Disable Apple's Full-Screen mode
	ns-use-native-fullscreen nil))

;; Required packages
(require 'alchemist)
(require 'column-marker)
(require 'company-c-headers)
(require 'ensime)
(require 'faces)
(require 'fill-column-indicator)
(require 'font-lock)
(require 'helm)
(require 'helm-config)
(require 'helm-grep)
(require 'helm-R)
(require 'inf-haskell)
(require 'javadoc-lookup)
(require 'linum)
(require 'projectile)
(require 'rvm)
(require 'recentf)
(require 'saveplace)
(require 'semantic)
(require 'sr-speedbar)
(require 'tls)
(require 'tramp)
(require 'uniquify)
(require 'web-mode)
(require 'yasnippet)

(load-init-el "hooks.el")
(load-init-el "keys.el")
(load-init-el "local-prefs.el")

(add-hook 'after-init-hook
          (lambda ()
            ;; setup some custom modes
            (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
            (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

            ;;(set-face-attribute 'default nil :family "Anonymous Pro" :weight 'normal :width 'normal :height 120)
            ;;(set-face-attribute 'default nil :family "Hack" :weight 'normal :width 'normal :height 100)

            (add-to-list 'default-frame-alist '(height . 40))
            (add-to-list 'default-frame-alist '(width . 120))

            (setq
             explicit-shell-file-name "/bin/bash"
             font-lock-maximum-decoration t
             frame-title-format '(buffer-file-name "%f" (dired-directory dired-directory "%b"))
             inhibit-startup-screen t
             linum-format "%d "
             magit-last-seen-setup-instructions "1.4.0"
             next-line-add-newlines nil
             save-place-file (concat user-emacs-directory "places")
             show-paren-style 'mixed
             standard-indent 4
             transient-mark-mode t
             truncate-partial-width-windows nil
             uniquify-buffer-name-style 'post-forward-angle-brackets
             visible-bell t
             whitespace-line-column 80
             whitespace-style '(face trailing tabs)
             x-select-enable-clipboard t)

            (setq-default
             buffer-file-coding-system 'undecided-unix
             fci-rule-column 120
             fill-column 80
             highlight-changes-mode t
             indicate-empty-lines t
             indent-tabs-mode nil
             save-place t
             savehist-mode t
             truncate-lines t
             tab-width 4)

            (setq android-mode-sdk-dir (concat (getenv "HOME") "/Development/android-sdk-macosx"))

            (setq
             bibtex-entry-format '(opts-or-alts realign last-comma delimiters page-dashes)
             bibtex-autokey-year-length 4
             bibtex-autokey-year-title-separator ""
             bibtex-autokey-titleword-length nil)

            (setq fci-handle-truncate-lines nil)

            (setq
             helm-quick-update                     t ; do not display invisible candidates
             helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
             helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
             helm-move-to-line-cycle-in-source     0 ; move to end or beginning of source when reaching top or bottom of source.
             helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
             helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
             helm-ff-file-name-history-use-recentf t)

            (setq matlab-indent-function t)

            (setq recentf-max-menu-items 25)

            (setq tramp-default-method "sshx")

            ;; coding system
            (prefer-coding-system 'utf-8)
            (setq locale-coding-system 'utf-8)
            (set-selection-coding-system 'utf-8)

            ;; enable and disable stuff
            (global-company-mode 1)
            (add-to-list 'company-backends 'company-anaconda)
            (add-to-list 'company-backends 'ensime-company)

            (global-font-lock-mode 1)
            (global-linum-mode t)

            (column-number-mode t)
            (line-number-mode t)
            (size-indication-mode t)

            ;; line highlighting
            (toggle-hl-line-when-idle -1)
            (global-hl-line-mode -1)

            (global-semantic-highlight-func-mode t)
            (global-semantic-highlight-edits-mode t)
            (global-semantic-stickyfunc-mode -1)
            (global-whitespace-mode t)

            (helm-mode 1)
            (recentf-mode 1)
            (show-paren-mode 1)

            (tool-bar-mode -1)
            (turn-on-font-lock)

            (projectile-global-mode 1)

            ;; set up YASnippet
            (add-to-list 'yas-snippet-dirs (concat user-emacs-directory "snippets"))
            (setq yas/also-auto-indent-first-line t)
            (yas/global-mode 1)

            (load-theme 'darcula t)

            (set-face-attribute 'default nil :family "Hack" :weight 'normal :width 'normal :height 100)
            ))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416c552" "4217c670c803e8a831797ccf51c7e6f3a9e102cb9345e3662cc449f4c194ed7d" "8fd393097ac6eabfcb172f656d781866beec05f27920a0691e8772aa2cdc7132" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" default)))
 '(package-selected-packages
   (quote
    (ag
     alchemist
     anaconda-mode
     android-mode
     ant
     auto-complete
     col-highlight
     column-marker
     company
     company-anaconda
     company-c-headers
     company-ghc
     company-go
     company-inf-ruby
     company-irony
     concurrent
     crosshairs
     ctable
     darcula-theme
     deferred
     dirtree
     elixir-mix
     elixir-mode
     elixir-yasnippets
     ensime
     epc
     fill-column-indicator
     flycheck
     flycheck-haskell
     flycheck-haskell
     flycheck-irony
     flycheck-pos-tip
     flymake
     flymake-elixir
     flymake-go
     flymake-haml
     flymake-json
     flymake-python-pyflakes
     flymake-ruby
     git-rebase-mode
     git-commit-mode
     go-eldoc
     go-mode
     go-projectile
     groovy-mode
     haskell-mode
     hc-zenburn-theme
     helm
     helm-R
     helm-ag-r
     helm-aws
     helm-bibtex
     helm-c-moccur
     helm-c-yasnippet
     helm-company
     helm-dash
     helm-flycheck
     helm-ghc
     helm-git
     helm-go-package
     helm-google
     helm-open-github
     helm-package
     helm-projectile
     helm-projectile-all
     helm-pydoc
     helm-rails
     helm-rb
     hl-line+
     inf-ruby
     irony
     javadoc-lookup
     jedi
     json-mode
     magit
     markdown-mode
     matlab-mode
     nasm-mode
     popup
     projectile
     projectile-speedbar
     projectile-rails
     python-environment
     rust-mode
     sr-speedbar
     redis
     rich-minority
     rubocop
     ruby-block
     ruby-electric
     ruby-hash-syntax
     ruby-tools
     rvm
     sbt-mode
     scala-mode2
     scala-outline-popup
     snippet
     vline
     web-mode
     yaml-mode
     yasnippet
     zenburn-theme))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
