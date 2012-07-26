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
  (setq explicit-shell-file-name "/bin/bash")

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
;;(require 'android-mode)
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

;; semantic is part of the cedet suite of tools prior to emacs 23.2, this was a separate package. I
;; don't want to bring all of CEDET into the repo now due to the setup process.
(when (require 'semantic nil 'noerror)
  (global-ede-mode 1)
  (semantic-mode 1))

;; Load up irc
(defun init-irc ()
  (interactive)
  (require 'erc)
  (require 'erc-match)
  (require 'erc-services)
  (require 'erc-fill)

  (defvar erc-insert-post-hook)

  (setq erc-auto-query 'window-noselect
        erc-autojoin-channels-alist '(("foonetic.net" "#xkcd")
                                      ("freenode.net" "#rubyonrails" "#django" "#java"))
        erc-echo-notices-in-minibuffer-flag t
        erc-hide-timestamps nil
        erc-keywords '("seggy" "segfaultzen")
        erc-save-buffer-on-part nil
        erc-save-queries-on-quit nil
        erc-log-insert-log-on-open t
        erc-log-channels t
        erc-log-channels-directory "~/.irclogs/"
        erc-log-write-after-send t
        erc-log-write-after-insert t
        erc-max-buffer-size 20000
        erc-truncate-buffer-on-save t
        tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                      "gnutls-cli -p %p %h"
                      "gnutls-cli -p %p %h --protocols ssl3"))

  ;; logging:
  (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
    (save-some-buffers t (lambda ()
                           (when (and (eq major-mode 'erc-mode)
                                      (not (null buffer-file-name)))))))

  (setq erc-nick-color-alist '())
  (setq erc-nick-color-list '("white" "yellow" "red" "purple" "orange" ;; standard colors
                              "magenta" "cyan" "brown" "blue")) ;; exclude green because that's the normal text color

  (defun erc-get-color-for-nick (nick)
    "Gets a color for NICK. If NICK is in erc-nick-color-alist,
use that color, else hash the nick and use a random color from
the pool"
    (or (cdr (assoc nick erc-nick-color-alist))
        (nth
         (mod (string-to-number
               (substring (md5 nick) 0 6) 16)
              (length erc-nick-color-list))
         erc-nick-color-list)))

  (defun erc-put-color-on-nick ()
    "Modifies the color of nicks according to erc-get-color-for-nick"
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "<\\([^>]*\\)>")
          (let ((nick (match-string 1)))
            (put-text-property (match-beginning 1) (match-end 1) 'face
                               (cons 'foreground-color
                                     (erc-get-color-for-nick nick)))))))

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

  (defun connect-irc ()
    "Connect to IRC."
    (interactive)
    (erc-tls :server "irc.foonetic.net" :port 6697
             :nick "seggy" :full-name "segfaultzen")
    (erc-tls :server "irc.freenode.net" :port 6697
             :nick "seggy" :full-name "segfaultzen"))

  (add-hook 'erc-join-hook '(lambda ()
                              (ncm-mode)))

  (add-hook 'erc-insert-post-hook '(lambda ()
                                     (erc-truncate-buffer)
                                     (erc-save-buffer-in-logs)))

  (add-hook 'erc-mode-hook '(lambda ()
                              (when (not (featurep 'xemacs))
                                (set (make-variable-buffer-local
                                      'coding-system-for-write)
                                     'emacs-mule))))

  (add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)

  (erc-scrolltobottom-enable)
  (erc-match-mode t)
  (erc-services-mode t))

;; Custom key bindings
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-o" 'complete)
(global-set-key "\M-+" 'word-count-mode)
(global-set-key (kbd "<C-tab>") 'yas/expand-from-trigger-key)


(delete ".svn/" completion-ignored-extensions)
(delete ".hg/" completion-ignored-extensions)
(delete ".git/" completion-ignored-extensions)

;; this prevents IDO from hiding IRC log files
(delete "\\`#" ido-ignore-files)

(setq
 ;; some bibtex settings that I like
 bibtex-entry-format '(opts-or-alts realign last-comma delimiters page-dashes)
 bibtex-autokey-year-length 4
 bibtex-autokey-year-title-separator ""
 bibtex-autokey-titleword-length nil

 ;; browse-url-browser-function 'browse-url-text-emacs
 default-frame-alist (append
                      '((width . 130)
                        (height . 45)
                        (left . 250)
                        (top . 50)
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
 printer-name "192.168.1.199"
 save-place-file (concat user-emacs-directory "places")
 show-paren-style 'parenthesis
 standard-indent 2
 tex-dvi-view-command "xdvi"
 tramp-default-method "sshx"
 transient-mark-mode t
 truncate-partial-width-windows nil
 uniquify-buffer-name-style 'post-forward-angle-brackets
 user-full-name "Mitchell Peabody"
 user-mail-address "mitchell.peabody@gmail.com"
 visible-bell t
 whitespace-line-column 100
 whitespace-style '(face trailing)
 x-select-enable-clipboard t)

;; set defaults for buffer local variables
(setq-default
 buffer-file-coding-system 'undecided-unix
 c-basic-offset 2
 fci-rule-column 120
 fill-column 100
 indicate-empty-lines t
 indent-tabs-mode nil
 save-place t
 savehist-mode t
 truncate-lines t
 tab-width 2)

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

;; set up YASnippet
(add-to-list 'yas/snippet-dirs (concat user-emacs-directory "snippets"))
(setq yas/also-auto-indent-first-line t)
(yas/initialize)
(yas/global-mode 1)

;; enable some features
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; set up some language specific mode hooks
(defun untabify-before-save ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  nil)

(add-hook 'java-mode-hook
          (lambda ()
            (add-hook 'write-contents-hooks 'untabify-before-save)
            (java-docs java-docs-directory)
            (java-mode-indent-annotations-setup)))

(add-hook 'latex-mode-hook
          (lambda ()
            (turn-on-reftex)))

(add-hook 'matlab-mode-hook
          (lambda ()
            (auto-fill-mode nil)))

(add-hook 'text-mode-hook
      (lambda ()
        (flyspell-mode t)
        (setq ;; tab-stop-list (number-sequence 2 100 2)
         indent-tabs-mode nil
         tab-width 2
         indent-line-function (quote insert-tab)
         fill-column 80)))

(add-hook 'change-log-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (setq fill-column 80)))

(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

(add-hook 'compilation-mode-hook
          '(lambda ()
             (setq truncate-lines 1)
             (setq truncate-partial-width-windows 1)))

(add-hook 'after-change-major-mode-hook '(lambda ()
                                           (if (display-graphic-p)
                                               (fci-mode 1))))

(defun config-frame (frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (scroll-bar-mode 0)
      (tool-bar-mode 0))))

(when (display-graphic-p)
  (config-frame (selected-frame)))

;; this hook sets up preferences that require a graphics display
(add-hook 'after-make-frame-functions
          'config-frame)

(add-hook 'compilation-mode-hook
          '(lambda ()
             (setq truncate-lines 1)
             (setq truncate-partial-width-windows 1)))

;;;;
(when (display-graphic-p)
  (defvar center-frame-size-margin-width 100)
  (defvar center-frame-size-margin-height 100)
  (defvar center-frame-top-fudge 170)
  (defvar center-frame-left-fudge 20)
  (defvar center-frame-char-height 50)
  (defvar center-frame-fill-column-padding 40)

  ;; rough cut of centering code for initial frame
  (defun set-frame-size-for-resolution ()
    (set-frame-height
     (selected-frame)
     (min
      (/ (- (x-display-pixel-height) center-frame-size-margin-width)
         (frame-char-height))
      center-frame-char-height))
    (set-frame-width
     (selected-frame)
     (min
      (/ (- (x-display-pixel-width) center-frame-size-margin-height)
         (frame-char-width))
      (+ fill-column center-frame-fill-column-padding)))
    (set-frame-position
     (selected-frame)
     (/
      (-
       (x-display-pixel-width)
       (+ (frame-pixel-width) center-frame-left-fudge))
      2)
     (/
      (-
       (x-display-pixel-height)
       (+ (frame-pixel-height) center-frame-top-fudge))
      2)))

  (set-frame-size-for-resolution))

;;   C-x n d ... narrow to def
;;   C-x n n ... narrow to region
;;   C-x n p ... narrow to page
;;   C-x n w ... widen back
