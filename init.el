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
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(package-initialize)

(load (concat user-emacs-directory "funcs.el"))

(add-hook 'after-init-hook (lambda () (load-init-el "realinit.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(custom-safe-themes
   (quote
    ("bce3ae31774e626dce97ed6d7781b4c147c990e48a35baedf67e185ebc544a56" "7e1fa2fd97e792390d0c2347f0eefa2d1679c68da56e6baf983b057cefa400b4" "551596f9165514c617c99ad6ce13196d6e7caa7035cea92a0e143dbe7b28be0e" "cc60d17db31a53adf93ec6fad5a9cfff6e177664994a52346f81f62840fe8e23" "7bc31a546e510e6bde482ebca992e293a54cb075a0cbfb384bf2bf5357d4dee3" "118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "9dae95cdbed1505d45322ef8b5aa90ccb6cb59e0ff26fef0b8f411dfc416
c552" "4217c670c803e8a831797ccf51c7e6f3a9e102cb9345e3662cc449f4c194ed7d" "8fd393097ac6eabfcb172f656d781866beec05f27920a0691e8772aa2cdc
7132" "3b819bba57a676edf6e4881bd38c777f96d1aa3b3b5bc21d8266fa5b0d0f1ebf" "f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b309
3e32" default)))
 '(package-selected-packages
   (quote
    (diff-hl magit-gh-pulls ruby-additional graphviz-dot-mode cherry-blossom-theme origami docker-compose-mode flycheck-elixir crontab-mode robe smartparens dockerfile-mode erlang csv-mode company-ghci eruby-mode haml-mode rspec-mode yard-mode chruby ruby-refactor floobits w3 w3m ag alchemist anaconda-mode android-mode ant auto-complete col-highlight column-marker company company-anaconda company-c-headers company-ghc company-go company-inf-ruby company-irony concurrent crosshairs ctable darcula-theme deferred dirtree elixir-mix elixir-mode ensime epc fill-column-indicator flycheck flycheck-haskell flycheck-haskell flycheck-irony flycheck-pos-tip flymake flymake-elixir flymake-go flymake-haml flymake-json flymake-python-pyflakes flymake-ruby go-eldoc go-mode go-projectile groovy-mode haskell-mode hc-zenburn-theme helm helm-R helm-ag-r helm-aws helm-bibtex helm-c-moccur helm-company helm-flycheck helm-ghc helm-git helm-go-package helm-google helm-package helm-projectile helm-pydoc helm-rails helm-rb hl-line+ inf-ruby irony javadoc-lookup jedi json-mode magit markdown-mode matlab-mode nasm-mode popup projectile python-environment rust-mode redis rich-minority rubocop ruby-block ruby-electric ruby-hash-syntax ruby-tools scala-mode2 scala-outline-popup snippet vline web-mode yaml-mode zenburn-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
