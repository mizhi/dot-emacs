;; TODO: Figure out what the problem with terminal mode is. (maybe
;; only an OSX thins?)

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
(require 'tls)
(require 'tramp)
(require 'uniquify)
(require 'yasnippet)

;; semantic is part of the cedet suite of tools prior to emacs 23.2,
;; this was a separate package. I don't want to bring this into the
;; repo at this time. Maybe later.
(when (require 'semantic nil 'noerror)
  (global-ede-mode 1)
  (semantic-mode 1))

;; Load up irc
(defun start-irc ()
  (interactive)
  (require 'erc)
  (require 'erc-match)

  (defvar erc-insert-post-hook)

  (setq erc-auto-query 'window-noselect
	erc-autojoin-channels-alist '(("foonetic.net" "#xkcd" "#xkcd-compsci"))
	erc-echo-notices-in-minibuffer-flag t
	erc-hide-timestamps nil
	erc-keywords '("seggy" "segfaultzen")
	erc-log-insert-log-on-open nil
	erc-log-channels t
	erc-log-channels-directory "~/.irclogs/"
	erc-max-buffer-size 20000
	erc-save-buffer-on-part t
	erc-truncate-buffer-on-save t
	tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
		      "gnutls-cli -p %p %h"
		      "gnutls-cli -p %p %h --protocols ssl3"))
  (erc-match-mode)

  ;; logging:
  (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
    (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
					       (not (null buffer-file-name)))))))

  (add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
  (add-hook 'erc-mode-hook '(lambda ()
			      (when (not (featurep 'xemacs))
				(set (make-variable-buffer-local
				      'coding-system-for-write)
				     'emacs-mule))
			      (erc-set-colors-list nil)))
  ;; end logging

  ;; Truncate buffers so they don't hog core.
  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)

  (define-minor-mode ncm-mode "" nil
    (:eval
     (let ((ops 0)
	   (voices 0)
	   (members 0))
       (maphash (lambda (key value)
		  (when (erc-channel-user-op-p key)
		    (setq ops (1+ ops)))
		  (when (erc-channel-user-voice-p key)
		    (setq voices (1+ voices)))
		  (setq members (1+ members)))
		erc-channel-users)
       (format " %S/%S/%S" ops voices members))))

  (add-hook 'erc-join-hook '(lambda () (ncm-mode)))

  ;; Pool of colors to use when coloring IRC nicks.
  (setq erc-nick-colors-list '("green" "blue" "red"
			       "dark gray" "dark orange"
			       "dark magenta" "maroon"
			       "indian red" "forest green"
			       "midnight blue" "dark violet"))
  ;; special colors for some people
  (setq erc-nick-color-alist '())

  (defun erc-set-colors-list (frame)
    (unless (frame-parameter frame 'erc-nick-colors-list)
      (let ((v nil))
	(dolist (color (defined-colors))
	  (or (string-match-p "black" color)
	      (add-to-list 'v color)))
	(set-frame-parameter frame 'erc-nick-colors-list v))))

  (defun erc-get-color-for-nick (nick)
    "Gets a color for NICK. If NICK is in erc-nick-color-alist,
use that color, else hash the nick and use a random color from
the pool"
    (let ((colors (frame-parameter nil 'erc-nick-colors-list)))
      (or (cdr (assoc nick erc-nick-color-alist))
	  (nth
	   (mod (string-to-number
		 (substring (md5 nick) 0 6) 16)
		(length colors))
	   colors))))

  (defun erc-put-color-on-nick ()
    "Modifies the color of nicks according to erc-get-color-for-nick"
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "<\\([^>]*\\)>")
	  (let ((nick (match-string 1)))
	    (put-text-property (match-beginning 1) (match-end 1) 'face
			       (cons 'foreground-color
				     (erc-get-color-for-nick nick)))))))


  (add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)
  (add-hook 'after-make-frame-functions
	    '(lambda (frame)
	       (erc-set-colors-list frame)))



  (defun connect-irc ()
    "Connect to IRC."
    (interactive)
    (erc-tls :server "irc.foonetic.net" :port 6697
	     :nick "seggy" :full-name "segfaultzen")))

;;    naos.foonetic.net, 443, 80
;;    (erc :server "irc.freenode.net" :port 6667
;;	 :nick "ootput" :full-name "ootput")


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
			(foreground-color . "green")
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
 save-place-file (concat user-emacs-directory "places")
 show-paren-style 'parenthesis
 standard-indent 2
 tab-width 2
 tex-dvi-view-command "xdvi"
 tramp-default-method "sshx"
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
 fill-column 100
 indicate-empty-lines t
 truncate-lines t
 save-place t
 savehist-mode t)

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

(yas/initialize)
(yas/load-directory (concat user-emacs-directory "elisp/yasnippet/snippets/"))
(yas/global-mode 1)


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

(defun config-frame (frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (scroll-bar-mode 0)
      (tool-bar-mode 0))))

(config-frame (selected-frame))

;; this hook sets up preferences that require a graphics display
(add-hook 'after-make-frame-functions
	  'config-frame)




;;   C-x n d ... narrow to def
;;   C-x n n ... narrow to region
;;   C-x n p ... narrow to page
;;   C-x n w ... widen back
