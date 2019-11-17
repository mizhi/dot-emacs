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

;; Set up emacs exec path
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; load some custom functions
(load (concat user-emacs-directory "funcs.el"))

;; separate custom-file from init.el
(setq custom-file (concat user-emacs-directory "custom-settings.el"))

(add-hook 'after-init-hook (lambda () (load-init-el "realinit.el")))

;;; init.el ends here
