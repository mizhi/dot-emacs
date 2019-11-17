;; hooks.el --- Various hooks for emacs

;;; Commentary:

;;; Code:
(add-hook 'prog-mode-hook
          (lambda ()
            (diff-hl-flydiff-mode 1)
            (flycheck-mode 1)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)))

(add-hook 'projectile-mode-hook
          (lambda ()
            (helm-projectile-on)
            (setq projectile-enable-caching t
                  projectile-remember-window-configs t
                  projectile-completion-system 'helm
                  projectile-switch-project-action 'helm-projectile
                  projectile-find-dir-includes-top-level t
                  projectile-enable-idle-timer t
                  projectile-globally-ignored-file-suffixes '(".min.js"))))
;;                  projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s")))

(add-hook 'change-log-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (setq fill-column 80)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'company-backends 'company-c-headers)
            (add-hook 'write-contents-hooks 'untabify-before-save)))

(add-hook 'go-mode-hook
          (lambda ()
            (add-hook 'before-save-hook 'gofmt-before-save)))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq js-indent-level 2)))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(add-hook 'python-mode-hook
          (lambda ()
            ;;(my:configure-python-venv)

            ;; discover location of python libs and add to db
            ;; (let
            ;;     ((python-lib-dir
            ;;       (concat (shell-command-to-string "python-config --prefix") "/lib/python2.7")))
            ;;   (semantic-add-system-include python-lib-dir 'python-mode))

            (setq fill-column 80
                  fci-rule-column 132
                  indent-tabs-mode nil
                  python-indent 4)

            (add-to-list 'company-backends 'company-anaconda)

            (electric-indent-local-mode 0)

            (flycheck-mode 0)
            (anaconda-mode 1)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ruby-insert-encoding-magic-comment nil)
            (setq ruby-deep-indent-paren nil)
            (setq ruby-align-chained-calls t)
            (setq ruby-deep-arglist nil)
            (setq ruby-align-to-stmt-keywords t)
            (turn-on-fci-mode)
            (chruby-use-corresponding)))

(add-hook 'ruby-mode-hook 'eldoc-mode)
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'rspec-mode)
(add-hook 'ruby-mode-hook 'rubocop-mode)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)
(add-hook 'ruby-mode-hook 'yard-mode)

(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (setq enh-ruby-deep-indent-paren nil)
            (setq enh-ruby-deep-arglist nil)
            (setq enh-ruby-bounce-deep-indent t)
            (turn-on-fci-mode)
            (chruby-use-corresponding)))

(add-hook 'enh-ruby-mode-hook 'eldoc-mode)
(add-hook 'enh-ruby-mode-hook 'flycheck-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'enh-ruby-mode-hook 'rspec-mode)
(add-hook 'enh-ruby-mode-hook 'rubocop-mode)
(add-hook 'enh-ruby-mode-hook 'ruby-tools-mode)
(add-hook 'enh-ruby-mode-hook 'yard-mode)

(add-hook 'scss-mode-hook
          (lambda ()
            (setq css-indent-offset 2)))

(add-hook 'text-mode-hook
          (lambda ()
            (setq fill-column 80
                  indent-line-function (quote insert-tab)
                  indent-tabs-mode nil
                  tab-width 2)))

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 2
                  web-mode-code-indent-offset 2
                  )))

(add-hook 'yaml-mode-hook
          (lambda ()
            (setq yaml-indent-offset 2)))

(add-hook 'compilation-mode-hook
          (lambda ()
            (setq truncate-lines 1)
            (setq truncate-partial-width-windows 1)))

;;; hooks.el ends here
