;; environment variables that are used throughout the configuration.
(setq
 explicit-shell-file-name "/bin/tcsh"
 user-home-directory (getenv "HOME")
 documents-path user-home-directory
 is-windows (eq system-type 'windows-nt)
 is-mac (eq system-type 'darwin))

;; send my backups here
(add-to-list 'backup-directory-alist
	     (cons ".*"
		   (concat user-emacs-directory "backups/")))

;; setup load path

(setq load-path
      (append
       (list
 	(concat user-emacs-directory "elisp/")) load-path))

(let ((default-directory (concat user-emacs-directory "elisp/")))
  (normal-top-level-add-subdirs-to-load-path))

(cond
 (is-windows
  (setq-default ispell-program-name
		"c:/Program Files (x86)/Aspell/bin/aspell.exe")
  (setq cygwin-mount-cygwin-bin-directory "c:/Cygwin/bin")
  (require 'cygwin-mount)
  (require 'setup-cygwin))
 (is-mac
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\.")))
  ;; if we're running Aqua Emacs, turn off the annoying tendency for
  ;; a new frame to be created when opening a buffer.
  (if (functionp 'one-buffer-one-frame-mode)
      (one-buffer-one-frame-mode))))

;; make LaTeX work... better way to do this?
(setenv "PATH"
	(concat (getenv "PATH") ":/usr/texbin"))

;; load some editing modes
(autoload 'bibtex-mode "bibtex" "Bibtex Mode")
(autoload 'c-mode "cc-mode" "C Editing Mode" t)
(autoload 'c++-mode   "cc-mode" "C++ Editing Mode" t)
(autoload 'c++-c-mode "cc-mode" "C++/C Editing Mode" t)
(autoload 'css-mode "css-mode-simple" "CSS Editing Mode" t)
(autoload 'go-mode "go-mode" "Go editing mode" t)
(autoload 'haskell-mode "haskell-mode" "Haskell editing mode" t)
(autoload 'javascript-mode "javascript" "Javascript editing mode" t)
(autoload 'LaTeX-mode "tex-mode" "LaTeX Mode" t)
(autoload 'matlab-mode "matlab" "Enter Matlab mode." t)
(autoload 'matlab-shell "matlab" "Interactive Matlab mode." t)
(autoload 'objc-mode "cc-mode" "Objective-C Editing Mode" t)
(autoload 'reftex-citation "reftex" "Do citation with RefTeX" t)
(autoload 'reftex-mode     "reftex" "RefTeX Minor Mode" t)
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby" t)
(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process" t)
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(autoload 'scheme-mode "scheme" "Scheme Mode" t)
(autoload 'shell-mode "sh-mode" "Shell Mode" t)
(autoload 'turn-on-reftex  "reftex" "RefTeX Minor Mode" nil)
(autoload 'word-count-mode "word-count" "Minor mode to count words." t nil)
(autoload 'yaml-mode "yaml-mode" "Yaml Mode" t)

;; auto load list for the modes.
(setq auto-mode-alist
      (append
       '(("\\.bib$" . bibtex-mode)
	 ("\\.c$"    . c-mode)
	 ("\\.h$"    . c-mode)
	 ("\\.cc$"   . c++-mode)
	 ("\\.cpp$"   . c++-mode) ;;VC++
	 ("\\.cxx$"   . c++-mode) ;;VC++
	 ("\\.hh$"   . c++-mode) ;;VC++
	 ("\\.hxx$"   . c++-mode) ;;VC++
	 ("\\.hs$" . haskell-mode)
	 ("\\.css$" . css-mode)
	 ("\\.cl$" . emacs-lisp-mode)
	 ("\\.el$" . emacs-lisp-mode)
	 ("\\.gwm$" . emacs-lisp-mode)
	 ("\\.lisp$" . emacs-lisp-mode)
	 ("\\.lsp$" . emacs-lisp-mode)
	 ("\\.vm$"   . emacs-lisp-mode)
	 ("\\.go$" . go-mode)
	 ("\\.java" . java-mode)
	 ("\\.js$" . javascript-mode)
	 ("\\.json$" . javascript-mode)
	 ("\\.tex" . LaTeX-mode)
	 ("\\.m$"    . objc-mode) ;; objective C
	 ("\\.pl" . perl-mode)
	 ("\\.php$" . php-mode)
	 ("\\.rb$" . ruby-mode)
	 ("\\.scm$"    . scheme-mode)
	 ("\\.st" . smalltalk-mode)
	 ("\\.exp$"   . tcl-mode)   ;;Expect
	 ("\\.tcl$"   . tcl-mode)
	 ("\\.yml$" . yaml-mode))
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
(require 'saveplace)
(require 'semantic)
(require 'tramp)
(require 'uniquify)

;; initialization functions
;; setup hooks for working with reftex when .tex files
;; are loaded.
(add-hook 'LaTeX-mode-hook 'turn-on-reftex) ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
(add-hook 'ruby-mode-hook '(lambda () (inf-ruby-keys)))
(add-hook 'java-mode-hook '(lambda ()
			     (java-mode-indent-annotations-setup)
			     (setq truncate-lines t)))
(add-hook 'matlab-mode-hook '(lambda () (auto-fill-mode nil)))

(add-hook 'text-mode-hook '(lambda()
			     (flyspell-mode t)       ; spellchek (sic) on the fl
			     (setq tab-stop-list (number-sequence 2 100 2)
				   indent-tabs-mode nil)))

;; nuke trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'after-change-major-mode-hook '(lambda ()
					   (fci-mode 1)))

;; Custom key bindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-o" 'complete)
(global-set-key "\M-+" 'word-count-mode)

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
			(foreground-color . "wheat")
			(background-color . "black")
			(font . "-outline-Anonymous Pro-normal-normal-normal-mono-16-*-*-*-c-*-iso8859-1"))
		      default-frame-alist)
 fci-handle-truncate-lines nil
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
 whitespace-line-column 80
 whitespace-style '(face trailing)
 x-select-enable-clipboard t)

;; set defaults for buffer local variables
(setq-default
 fci-rule-column 120
 indicate-empty-lines t
 truncate-lines nil)

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
(global-ede-mode 1)
;;(ido-mode t)
(scroll-bar-mode 0)
(semantic-mode 1)
(show-paren-mode 1)
(tool-bar-mode 0)
(turn-on-font-lock)

;; enable some features
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; this needs to go in a better place
(java-docs "/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/docs/api")
