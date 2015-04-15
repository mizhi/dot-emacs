(add-hook 'desktop-after-read-hook
          '(lambda ()
             ;; desktop-remove clears desktop-dirname
             (setq desktop-dirname-tmp desktop-dirname)
             (desktop-remove)
             (setq desktop-dirname desktop-dirname-tmp)))

(add-hook 'prog-mode-hook
          (lambda ()
            (rvm-autodetect-ruby)
            (flycheck-mode 1)))

(add-hook 'projectile-mode-hook
          (lambda ()
            (helm-projectile-on)
            (setq projectile-indexing-method 'native
                  projectile-enable-caching t
                  projectile-remember-window-configs t
                  projectile-completion-system 'helm
                  projectile-switch-project-action 'helm-projectile
                  projectile-find-dir-includes-top-level t)))

(add-hook 'change-log-mode-hook
          (lambda ()
            (auto-fill-mode 1)
            (setq fill-column 80)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (add-to-list 'company-c-headers-path-system "/usr/include/c++/4.8/")
            (add-to-list 'company-backends 'company-c-headers)
            (add-hook 'write-contents-hooks 'untabify-before-save)
            (setq c-basic-offset 4)))

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
            (rubocop-mode 1)
            (ruby-tools-mode 1)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (setq ruby-deep-indent-paren nil)))

(add-hook 'scala-mode-hook
          (lambda ()
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
