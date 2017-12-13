;; send my backups to a subdirectory under my emacs directory
(let ((user-backup-directory (concat user-emacs-directory "backups/")))
  (setq backup-directory-alist `((".*" . ,user-backup-directory)))
  (setq auto-save-file-name-transforms `((".*" ,user-backup-directory t))))

;; setup load path
(let ((default-directory (concat user-emacs-directory "elisp/")))
  (add-to-list 'load-path default-directory)
  (normal-top-level-add-subdirs-to-load-path))

(load-init-el "funcs.el")

;; platform specific configuration
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin")

  (require 'server)
  (when (and window-system (not (server-running-p)))
    (server-start))

  (setq-default ispell-program-name
		(first-existing-file '("/opt/local/bin/ispell" "/usr/local/bin/ispell")))
  (setq latex-run-command "/usr/texbin/latex"
	;; Disable Apple's Full-Screen mode
	ns-use-native-fullscreen nil))

;; Required packages
(require 'alchemist)
(require 'chruby)
(require 'column-marker)
(require 'company-c-headers)
(require 'fill-column-indicator)
(require 'helm)
(require 'helm-config)
(require 'helm-grep)
(require 'helm-R)
(require 'javadoc-lookup)
(require 'linum)
(require 'projectile)
(require 'recentf)
(require 'saveplace)
(require 'semantic)
(require 'smartparens-config)
(require 'smartparens-ruby)
(require 'tls)
(require 'tramp)
(require 'uniquify)
(require 'web-mode)

(load-init-el "hooks.el")
(load-init-el "keys.el")
(load-init-el "local-prefs.el")

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
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; (add-to-list 'auto-mode-alist
;;              '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))
;; (add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

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
 fci-rule-column 80
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

(setq tags-revert-without-query 1)

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
(global-hl-line-mode 1)
(set-face-background hl-line-face "color-238")

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
(projectile-rails-global-mode)

(load-theme 'darcula t)
;;(load-theme 'leuven t)

(set-face-attribute 'default nil :family "Hack" :weight 'normal :width 'normal :height 110)
