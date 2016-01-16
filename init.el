;; user-emacs-directory wasn't defined until later
;; versions. We'll try to fix it up here by defaulting
;; to ${HOME}/.emacs.d

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
  (setq backup-directory-alist
        `((".*" . ,user-backup-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,user-backup-directory t))))

;; setup load path
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(let ((default-directory (concat user-emacs-directory "elisp/")))
  (normal-top-level-add-subdirs-to-load-path))

(defun load-init-el (filename)
  (load (concat user-emacs-directory filename)))

;; use package management when in emacs >= 24. Is this a good idea? I don't know.
(when (>= emacs-major-version 24)
  (load-init-el "packages.el"))

(load-init-el "funcs.el")

(setq explicit-shell-file-name "/bin/bash")

;; platform specific configuration
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin")

  (when window-system
    (server-start))

  (setq-default ispell-program-name
                (first-existing-file '("/opt/local/bin/ispell" "/usr/local/bin/ispell")))
  (setq explicit-shell-file-name "/bin/bash"
        latex-run-command "/usr/texbin/latex"

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
(require 'ido)
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
            (set-face-attribute 'default nil :family "Hack" :weight 'normal :width 'normal :height 100)


            (add-to-list 'default-frame-alist '(height . 40))
            (add-to-list 'default-frame-alist '(width . 120))
            (setq
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
            (setq yas-installed-snippets-dir (concat user-emacs-directory "/elpa/yasnippet-20141005.124/snippets"))
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
    (web-mode darcula-theme rust-mode redis alchemist zenburn-theme sr-speedbar snippet rvm ruby-tools ruby-hash-syntax ruby-electric ruby-block rubocop rich-minority projectile-rails nasm-mode matlab-mode json-mode jedi javadoc-lookup ido-vertical-mode helm-rb helm-rails helm-pydoc helm-projectile-all helm-package helm-open-github helm-google helm-go-package helm-git helm-ghc helm-flycheck helm-company helm-c-yasnippet helm-c-moccur helm-bibtex helm-R hc-zenburn-theme groovy-mode go-projectile git-rebase-mode git-commit-mode flymake-ruby flymake-python-pyflakes flymake-json flymake-haml flymake-go flymake-elixir flymake flycheck-pos-tip flycheck-irony flycheck-haskell fill-column-indicator elixir-yasnippets elixir-mix dirtree crosshairs company-irony company-inf-ruby company-ghc company-c-headers company-anaconda column-marker ant android-mode ag))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
