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

;; send my backups here
(add-to-list 'backup-directory-alist (cons ".*" (concat user-emacs-directory "backups/")))

;; setup load path
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(let ((default-directory (concat user-emacs-directory "elisp/")))
  (normal-top-level-add-subdirs-to-load-path))

(cond
 ;; I use cygwin on windows machines when I can. Wonder if it's
 ;; possible to make this detect if I am using cygwin on a new
 ;; machine?
 (is-windows
  (setq-default ispell-program-name
		"c:/Program Files (x86)/Aspell/bin/aspell.exe")
  (setq cygwin-mount-cygwin-bin-directory "c:/Cygwin/bin")
  (setq java-docs-directory "c:/Java/32bit/jdk1.6.0_29/docs/api")
  (setq explicit-shell-file-name "c:/Cygwin/bin/bash")
  (require 'cygwin-mount)
  (require 'setup-cygwin))
 (is-mac
  ;; Work around a bug (?) on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\.")))
  (setq java-docs-directory "/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/docs/api")
  (setq explicit-shell-file-name "/bin/tcsh")

  (setq latex-run-command "/usr/texbin/latex")))

;; load some editing modes
(autoload 'go-mode "go-mode" "Go editing mode" t)
(autoload 'haskell-mode "haskell-mode" "Haskell editing mode" t)
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby" t)
(autoload 'word-count-mode "word-count" "Minor mode to count words." t)
(autoload 'yaml-mode "yaml-mode" "Yaml Mode" t)

;; auto load list for the modes.
(setq auto-mode-alist
      (append
       '(("\\.go\\'" . go-mode)
	 ("\\.hs\\'" . haskell-mode)
	 ("\\.js\\'" . javascript-mode)
	 ("\\.json\\'" . javascript-mode)
	 ("\\.rb\\'" . ruby-mode)
	 ("\\.yml\\'" . yaml-mode))
       auto-mode-alist))

;; Required packages
(require 'cl)
(require 'column-marker)
(require 'completion)
(require 'faces)
(require 'fill-column-indicator)
(require 'font-lock)
(require 'ido)
(require 'java-docs)
(require 'java-mode-indent-annotations)
(require 'java-mode-plus)
(require 'rails-autoload)
(require 'saveplace)
(require 'tramp)
(require 'uniquify)

;; semantic is part of the cedet suite of tools prior to emacs 23.2,
;; this was a separate package. I don't want to bring this into the
;; repo at this time. Maybe later.
(when (require 'semantic nil 'noerror)
  (global-ede-mode 1)
  (semantic-mode 1))

;; Custom key bindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-o" 'complete)
(global-set-key "\M-+" 'word-count-mode)

(delete ".svn/" completion-ignored-extensions)
(delete ".hg/" completion-ignored-extensions)
(delete ".git/" completion-ignored-extensions)

(setq
 ;; some bibtex settings that I like
 bibtex-entry-format '(opts-or-alts realign last-comma delimiters page-dashes)
 bibtex-autokey-year-length 4
 bibtex-autokey-year-title-separator ""
 bibtex-autokey-titleword-length nil

 ;; browse-url-browser-function 'browse-url-text-emacs
 default-buffer-file-coding-system 'undecided-unix
 default-frame-alist (append
		      '((width . 130)
			(height . 50)
			(left . 250)
			(top . 85)
;;			(foreground-color . "wheat")
			(foreground-color . "green")
			(background-color . "black")
			(font . "-outline-Anonymous Pro-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1"))
		      default-frame-alist)
 fci-handle-truncate-lines nil
 fill-column 100
 font-lock-maximum-decoration t
 frame-title-format '(buffer-file-name "%f"
				       (dired-directory dired-directory "%b"))
 global-semantic-highlight-func-mode t
 global-semantic-highlight-edits-mode t
 inhibit-startup-screen t
 matlab-indent-function t
 next-line-add-newlines nil
 printer-name ""
 save-place t
 save-place-file (concat user-emacs-directory "places")
 show-paren-style 'parenthesis
 standard-indent 2
 tab-width 2
 tex-dvi-view-command "xdvi"
 tramp-default-method "ssh"
 transient-mark-mode t
 truncate-partial-width-windows nil
 uniquify-buffer-name-style 'post-forward-angle-brackets
 visible-bell t
 whitespace-line-column 100
 whitespace-style '(face trailing)
 x-select-enable-clipboard t)

;; set defaults for buffer local variables
(setq-default
 fci-rule-column 120
 indicate-empty-lines t
 truncate-lines t)

;; coding system
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; General functions that need to be called
(column-number-mode t)
(completion-initialize)
(global-font-lock-mode 1)
(global-whitespace-mode t)
(ido-mode t)
(show-paren-mode 1)
(turn-on-font-lock)

;; enable some features
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; set up some language specific mode hooks
(add-hook 'java-mode-hook
	  (lambda ()
	    (java-docs java-docs-directory)
	    (java-mode-indent-annotations-setup)))

(add-hook 'latex-mode-hook
	  (lambda ()
	    (turn-on-reftex)))

(add-hook 'matlab-mode-hook
	  (lambda ()
	    (auto-fill-mode nil)))

(add-hook 'text-mode-hook
	  (lambda()
	    (flyspell-mode t)
	    (setq tab-stop-list (number-sequence 2 100 2)
		  indent-tabs-mode nil)))

;; nuke trailing whitespace
(add-hook 'before-save-hook
	  (lambda ()
	    (delete-trailing-whitespace)))

(add-hook 'after-change-major-mode-hook '(lambda ()
					   (if (display-graphic-p)
					       (fci-mode 1))))

;; this hook sets up preferences that require a graphics display
(add-hook 'after-make-frame-functions
	  (lambda ()
	    (if (display-graphic-p)
		((scroll-bar-mode 0)
		 (tool-bar-mode 0)))))

