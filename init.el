;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

(defun load-init-el (filename)
  (load (concat user-emacs-directory filename)))

(add-hook 'after-init-hook (lambda () (load-init-el "realinit.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416
c552" "4217c670c803e8a831797ccf51c7e6f3a9e102cb9345e3662cc449f4c194ed7d" "8fd393097ac6eabfcb172f656d781866beec05f27920a0691e8772aa2cdc
7132" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b309
3e32" default)))
 '(package-selected-packages
   (quote
    (ag alchemist anaconda-mode android-mode ant auto-complete col-highlight column-marker company company-anaconda company-c-headers company-ghc company-go company-inf-ruby company-irony concurrent crosshairs ctable darcula-theme deferred dirtree elixir-mix elixir-mode ensime epc fill-column-indicator flycheck flycheck-haskell flycheck-haskell flycheck-irony flycheck-pos-tip flymake flymake-elixir flymake-go flymake-haml flymake-json flymake-python-pyflakes flymake-ruby go-eldoc go-mode go-projectile groovy-mode haskell-mode hc-zenburn-theme helm helm-R helm-ag-r helm-aws helm-bibtex helm-c-moccur helm-company helm-dash helm-flycheck helm-ghc helm-git helm-go-package helm-google helm-open-github helm-package helm-projectile helm-pydoc helm-rails helm-rb hl-line+ inf-ruby irony javadoc-lookup jedi json-mode magit markdown-mode matlab-mode nasm-mode popup projectile projectile-rails python-environment rust-mode redis rich-minority rubocop ruby-block ruby-electric ruby-hash-syntax ruby-tools rvm sbt-mode scala-mode2 scala-outline-popup snippet vline web-mode yaml-mode zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
