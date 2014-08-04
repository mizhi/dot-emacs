;; environment variables that are used throughout the configuration.
(setq
 is-windows (eq system-type 'windows-nt)
 is-mac (eq system-type 'darwin))

;; user-emacs-directory wasn't defined until later
;; versions. We'll try to fix it up here by defaulting
;; to ${HOME}/.emacs.d
(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory
          (concat (getenv "HOME") "/.emacs.d/")))

(cd (getenv "HOME"))

;; send my backups here
(add-to-list 'backup-directory-alist
             (cons "" (concat user-emacs-directory "backups/")))

;; setup load path
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(let ((default-directory (concat user-emacs-directory "elisp/")))
  (normal-top-level-add-subdirs-to-load-path))

(defun load-init-el (filename)
  (load (concat user-emacs-directory filename)))

(load-init-el "funcs.el")
(load-init-el "hooks.el")
(load-init-el "keys.el")
(load-init-el "local-prefs.el")

;; use package management when in emacs >= 24. Is this a good idea? I don't know.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/") t)

  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  (mapc
   (lambda (package)
     (or (package-installed-p package)
         (if (y-or-n-p (format "Package %s is missing. Install it? " package))
             (package-install package))))
   '(ac-inf-ruby
     afternoon-theme
     ag
     android-mode
     ant
     auto-complete
     col-highlight
     color-theme
     column-marker
     concurrent
     crosshairs
     css-mode
     ctable
     deferred
     ensime
     epc
     fill-column-indicator
     flx
     flx-ido
     git-commit-mode
     git-rebase-mode
     go-eldoc
     go-mode
     go-projectile
     groovy-mode
     haskell-mode
     hl-line+
     inf-ruby
     javadoc-lookup
     jedi
     magit
     markdown-mode
     matlab-mode
     popup
     projectile
     python-environment
     sbt-mode
     scala-mode2
     snippet
     vline
     yaml-mode
     yasnippet)))

(cond
 (is-windows
  (setq-default ispell-program-name "c:/Program Files (x86)/Aspell/bin/aspell.exe")

  (if (file-exists-p "c:/Cygwin/bin")
      (setq cygwin-mount-cygwin-bin-directory "c:/Cygwin/bin")
    (setq explicit-shell-file-name "c:/Cygwin/bin/bash")
    (require 'cygwin-mount)
    (require 'setup-cygwin)))

 (is-mac
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin")

  (setq-default ispell-program-name
                (first-existing-file
                 '("/opt/local/bin/ispell"
                   "/usr/local/bin/ispell")))

  ;; Work around a bug (?) on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\.")))
  (setq explicit-shell-file-name "/bin/bash")
  (setq latex-run-command "/usr/texbin/latex")

  ;; I despise Mountain Lion's Full-Screen mode
  (setq ns-use-native-fullscreen nil)))


;; setup some autoloads.
(autoload 'android-mode "android-mode" "Android editing mode" t)
(autoload 'go-mode "go-mode" "Go editing mode" t)
(autoload 'haskell-mode "haskell-mode" "Haskell editing mode" t)
(autoload 'matlab-mode "matlab" "Enter Matlab mode" t)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode" t)
(autoload 'puppet-mode "puppet-mode" "Puppet Manifest editing mode" t)
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby" t)
(autoload 'scala-mode "scala-mode" "Mode for editing scala" t)
(autoload 'word-count-mode "word-count" "Minor mode to count words" t)
(autoload 'yaml-mode "yaml-mode" "Yaml Mode" t)

;; auto load list for the modes.
(setq auto-mode-alist
      (append
       '(("\\.go\\'" . go-mode)
         ("\\.hs\\'" . haskell-mode)
         ("\\.html\\'" . nxml-mode)
         ("\\.jj\\'" . javacc-mode)
         ("\\.js\\'" . javascript-mode)
         ("\\.json\\'" . javascript-mode)
         ("\\.pp\\'" . puppet-mode)
         ("\\.ya?ml\\'" . yaml-mode))
       auto-mode-alist))

;; Required packages
(require 'cl)
(require 'column-marker)
(require 'faces)
;;(require 'fill-column-indicator)
(require 'font-lock)
(require 'ido)
(require 'flx-ido)
(require 'inf-haskell)
(require 'javacc-mode)
(require 'javadoc-lookup)
(require 'linum)
(require 'magit)
(require 'projectile)
(require 'rails-autoload)
(require 'saveplace)
(require 'tls)
(require 'tramp)
(require 'uniquify)
(require 'whattf-dt)
(require 'yasnippet)

;; semantic is part of the cedet suite of tools prior to emacs 23.2, this was a
;; separate package. I don't want to bring all of CEDET into the repo now due to
;; the setup process.
(when (require 'semantic/ia nil 'noerror)
  (require 'semantic/bovine/gcc)

  (setq semantic-default-submodes
        (list 'global-semanticdb-minor-mode
              'global-semantic-highlight-func-mode
              'global-semantic-stickyfunc-mode
              'global-semantic-decoration-mode
              'global-semantic-idle-local-symbol-highlight-mode
              'global-semantic-idle-scheduler-mode
              'global-semantic-idle-completions-mode
              'global-semantic-idle-summary-mode
              'global-ede-mode))

  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)
  (semantic-mode 1))

;; setup relaxNG to know where html5 schemas are.
(eval-after-load "rng-loc"
  '(progn
     (add-to-list 'rng-schema-locating-files
                  (concat user-emacs-directory "elisp/html5-el/schemas.xml"))))

(delete ".svn/" completion-ignored-extensions)
(delete ".hg/" completion-ignored-extensions)
(delete ".git/" completion-ignored-extensions)

;; this prevents IDO from hiding IRC log files
(delete "\\`#" ido-ignore-files)

(set-face-attribute 'default nil
                    :family "Anonymous Pro" :weight 'normal :width 'normal :height 200)

(setq
 android-mode-sdk-dir (concat (getenv "HOME") "/Development/android-sdk-macosx")

 ;; some bibtex settings that I like
 bibtex-entry-format '(opts-or-alts realign last-comma delimiters page-dashes)
 bibtex-autokey-year-length 4
 bibtex-autokey-year-title-separator ""
 bibtex-autokey-titleword-length nil

 default-frame-alist (append
                      '((width . 110)
                        (height . 40)
                        (left . 200)
                        (top . 50))
                      default-frame-alist)
 fci-handle-truncate-lines nil
 font-lock-maximum-decoration t
 frame-title-format '(buffer-file-name
                      "%f"
                      (dired-directory dired-directory "%b"))
 ido-enable-flex-matching t
 ido-use-faces nil
 inhibit-startup-screen t
 linum-format "%d "
 matlab-indent-function t
 next-line-add-newlines nil
 save-place-file (concat user-emacs-directory "places")
 show-paren-style 'parenthesis
 standard-indent 4
 tex-dvi-view-command "xdvi"
 tramp-default-method "sshx"
 transient-mark-mode t
 truncate-partial-width-windows nil
 uniquify-buffer-name-style 'post-forward-angle-brackets
 user-full-name "Mitchell Peabody"
 visible-bell t
 whitespace-line-column 80
 whitespace-style '(face trailing)
 x-select-enable-clipboard t)

;; set defaults for buffer local variables
(setq-default
 buffer-file-coding-system 'undecided-unix
 c-basic-offset 4
 fci-rule-column 120
 fill-column 80
 indicate-empty-lines t
 indent-tabs-mode nil
 save-place t
 savehist-mode t
 truncate-lines t
 tab-width 4)

;; coding system
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; General functions that need to be called
(global-font-lock-mode 1)
(global-linum-mode t)
(column-number-mode t)

;; line highlighting
(global-hl-line-mode nil)
(toggle-hl-line-when-idle t)

(global-semantic-highlight-func-mode t)
(global-semantic-highlight-edits-mode t)
(global-whitespace-mode t)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)

(show-paren-mode 1)
(turn-on-font-lock)

;; set up YASnippet
(add-to-list 'yas/snippet-dirs (concat user-emacs-directory "snippets"))
(setq yas/also-auto-indent-first-line t)
(yas/global-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'afternoon)
