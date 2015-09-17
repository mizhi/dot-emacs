(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmelade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
 '(alchemist
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
   elixir-mix
   elixir-mode
   elixir-yasnippets
   ensime
   epc
   fill-column-indicator
   flycheck
   flycheck-haskell
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
   helm-flycheck
   helm-ghc
   helm-git
   helm-go-package
   helm-google
   helm-package
   helm-projectile
   helm-projectile-all
   helm-pydoc
   helm-rails
   helm-rb
   hl-line+
   inf-ruby
   irony
   javadoc-lookup
   jedi
   json-mode
   magit
   markdown-mode
   matlab-mode
   nasm-mode
   popup
   projectile
   projectile-speedbar
   projectile-rails
   python-environment
   sr-speedbar
   redis
   rubocop
   ruby-block
   ruby-electric
   ruby-hash-syntax
   ruby-tools
   rvm
   sbt-mode
   scala-mode2
   scala-outline-popup
   snippet
   vline
   yaml-mode
   yasnippet
   zenburn-theme))
