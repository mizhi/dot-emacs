(add-hook 'prog-mode-hook
          (lambda ()
            (flycheck-mode 1)

            (add-hook 'before-save-hook
                      (lambda ()
                        (delete-trailing-whitespace)) nil 'local)))

(add-hook 'projectile-mode-hook
          (lambda ()
            (helm-projectile-on)
            (setq projectile-indexing-method 'native
                  projectile-enable-caching t
                  projectile-remember-window-configs t
                  projectile-completion-system 'helm
                  projectile-switch-project-action 'helm-projectile
                  projectile-find-dir-includes-top-level t
                  projectile-enable-idle-timer t
                  projectile-globally-ignored-file-suffixes '(".min.js")
                  projectile-tags-command "ctags-exuberant -Re -f \"%s\" %s")))

(add-hook 'change-log-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (setq fill-column 80)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'company-backends 'company-c-headers)
            (add-hook 'write-contents-hooks 'untabify-before-save)
            (setq c-basic-offset 4)))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++11")
            (add-to-list 'company-c-headers-path-system "/usr/include/c++/")))

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
                  comment-style 'multi-line
                  js-indent-level 2)))

(add-hook 'json-mode-hook
          (lambda ()
            (make-local-variable 'js-indent-level)
            (setq js-indent-level 2)))

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

;; (defvar my:virtualenv-directory "~/.virtualenvs/"
;;   "The directory of virtualenvs.")

;; (defun my:configure-python-venv ()
;;   "Set `python-shell-virtualenv-path' to the virtualenv directory."
;;   (interactive)
;;   (require 'projectile)
;;   (let* ((project-name (projectile-project-name))
;;          (virtualenv-path
;;           (file-truename
;;            (concat my:virtualenv-directory project-name))))
;;     (when (file-directory-p virtualenv-path)
;;       (setq python-shell-virtualenv-path virtualenv-path))))

(add-hook 'python-mode-hook
          (lambda ()
            ;;(my:configure-python-venv)

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

            (flycheck-mode 0)
            (anaconda-mode 1)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ruby-insert-encoding-magic-comment nil)
            (setq ruby-deep-indent-paren nil)
            (setq ruby-align-chained-calls t)
            (rubocop-mode 1)
            (ruby-tools-mode 1)
            (yard-mode 1)
            (eldoc-mode 1)
            (flycheck-mode 1)
            (rspec-mode 1)
            (turn-on-fci-mode)))

(add-hook 'scala-mode-hook
          (lambda ()
            (ensime-scala-mode-hook)
            (local-set-key (kbd "\C-c o p") 'scala-outline-popup)))

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
            (setq web-mode-markup-indent-offset 2)))

(add-hook 'yaml-mode-hook
          (lambda ()
            (setq yaml-indent-offset 2)))

(add-hook 'compilation-mode-hook
          (lambda ()
            (setq truncate-lines 1)
            (setq truncate-partial-width-windows 1)))
