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
 '(ag
   anaconda-mode
   android-mode
   ant
   auto-complete
   col-highlight
   column-marker
   company
   company-anaconda
   company-c-headers
   company-ghc
   company-go
   company-inf-ruby
   concurrent
   crosshairs
   ctable
   deferred
   ensime
   epc
   fill-column-indicator
   flycheck
   flycheck-haskell
   flymake
   flymake-easy
   flymake-elixir
   flymake-go
   flymake-haml
   flymake-json
   flymake-python-pyflakes
   flymake-ruby
   git-commit-mode
   git-rebase-mode
   go-eldoc
   go-mode
   go-projectile
   groovy-mode
   haskell-mode
   hc-zenburn-theme
   helm
   helm-R
   helm-ag-r
   helm-aws
   helm-bibtex
   helm-c-moccur
   helm-c-yasnippet
   helm-company
   helm-dash
   helm-git
   helm-go-package
   helm-projectile
   helm-projectile-all
   helm-pydoc
   helm-rails
   helm-rb
   hl-line+
   ido-vertical-mode
   inf-ruby
   javadoc-lookup
   jedi
   json-mode
   magit
   markdown-mode
   matlab-mode
   popup
   projectile
   python-environment
   rubocop
   ruby-block
   ruby-electric
   ruby-hash-syntax
   ruby-tools
   rvm
   sbt-mode
   scala-mode2
   snippet
   vline
   yaml-mode
   yasnippet
   zenburn-theme))
