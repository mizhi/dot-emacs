;; Magit bindings
(define-prefix-command 'magit-map)
(global-set-key (kbd "C-c g") 'magit-map)
(define-key magit-map (kbd "s") 'magit-status)
(define-key magit-map (kbd "b") 'magit-blame-mode)

;; Some convenience things
(global-set-key [f2] 'visit-ansi-term)
(global-set-key [f7] 'newsticker-show-news)
(global-set-key [f8] 'toggle-frame-fullscreen)

(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-+" 'word-count-mode)
(global-set-key (kbd "<C-tab>") 'yas/expand-from-trigger-key)

(global-set-key (kbd "<M-S-up>") 'switch-to-prev-buffer)
(global-set-key (kbd "<M-S-down>") 'switch-to-next-buffer)

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z


;;(define-key projectile-mode-map (kbd "g") 'projectile-grep)

(global-set-key "\C-c\C-r" 'recentf-open-files)



;; re-enable some functions
;;   C-x n d ... narrow to def
;;   C-x n n ... narrow to region
;;   C-x n p ... narrow to page
;;   C-x n w ... widen back
;; enable some features
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(put 'scroll-left 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(windmove-default-keybindings)
