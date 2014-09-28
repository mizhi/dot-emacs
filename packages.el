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
 '(ac-c-headers
   ac-inf-ruby
   ac-python
   afternoon-theme
   ag
   android-mode
   ant
   auto-complete
   col-highlight
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
   go-autocomplete
   go-eldoc
   go-mode
   go-projectile
   groovy-mode
   haskell-mode
   hl-line+
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
   ruby-hash-syntax
   sbt-mode
   scala-mode2
   snippet
   vline
   yaml-mode
   yasnippet))
