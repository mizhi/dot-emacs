(add-hook 'prog-mode-hook
          (lambda ()
            (flymake-mode 1)
            (flycheck-mode 1)))

(add-hook 'projectile-mode-hook
          (lambda ()
            (setq projectile-indexing-method 'native)
            (setq projectile-enable-caching t)))

(add-hook 'change-log-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (setq fill-column 80)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-hook 'write-contents-hooks 'untabify-before-save)
            (setq c-basic-offset 4 )))

(add-hook 'java-mode-hook
          (lambda ()
            (when (require 'java-docs nil 'noerror)
              (java-docs java-docs-directory))

            (when (require 'java-mode-indent-annotations nil 'noerror)
              (java-mode-indent-annotations-setup))))

(add-hook 'javascript-mode-hook
          (lambda ()
            (setq comment-start "/*"
                  comment-end "*/"
                  comment-continue "*"
                  comment-style 'multi-line)))

(add-hook 'latex-mode-hook
          (lambda ()
            (turn-on-reftex)))

(add-hook 'matlab-mode-hook
          (lambda ()
            (auto-fill-mode nil)))

(add-hook 'nxml-mode-hook
          (lambda ()
            (define-key nxml-mode-map (kbd "<tab>") '(lambda () (interactive) (nxml-indent-line)))
            (define-key nxml-mode-map (kbd "C-S-o") 'nxml-complete)
            (setq nxml-slash-auto-complete-flag t)))

(add-hook 'python-mode-hook
          (lambda ()
            ;; discover location of python libs and add to db
            (let
                ((python-lib-dir
                  (concat (shell-command-to-string "python-config --prefix") "/lib/python2.7")))
              (semantic-add-system-include python-lib-dir 'python-mode))

            (setq fill-column 80
                  fci-rule-column 80
                  indent-tabs-mode nil
                  python-indent 4)

            (add-to-list 'company-backends 'company-anaconda)

            (electric-indent-local-mode 0)
            (fci-mode 1)
            (anaconda-mode 1)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (rubocop-mode 1)
            (ruby-tools-mode 1)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ruby-deep-indent-paren nil)))

(add-hook 'scala-mode-hook
          (lambda ()
            (require 'ensime)
            (ensime-scala-mode-hook)))


(add-hook 'term-mode-hook
          (lambda ()
            (setq yas-dont-activate t)))

(add-hook 'text-mode-hook
          (lambda ()
            (setq fill-column 80
                  indent-line-function (quote insert-tab)
                  indent-tabs-mode nil
                  tab-width 2)))

(add-hook 'yaml-mode-hook
          (lambda ()
            (setq yaml-indent-offset 2)))

;; General hooks
(add-hook 'before-save-hook
          (lambda ()
            (delete-trailing-whitespace)))

(add-hook 'compilation-mode-hook
          '(lambda ()
             (setq truncate-lines 1)
             (setq truncate-partial-width-windows 1)))

(when (display-graphic-p)
  (add-hook 'compilation-mode-hook
            '(lambda ()
               (setq truncate-lines 1)
               (setq truncate-partial-width-windows 1))))
