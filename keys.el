(global-set-key (kbd "C-c g s") 'magit-status)
(global-set-key (kbd "C-c g b") 'magit-blame)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Some convenience things
(global-set-key [f2] 'visit-ansi-term)
(global-set-key [f4] 'sr-speedbar-toggle)
(global-set-key [f8] 'toggle-fullscreen)

(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-+" 'word-count-mode)

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "C-<tab>") nil)
;;(global-set-key (kbd "<C-tab>") 'yas-expand-from-trigger-key)

(global-set-key (kbd "<M-S-up>") 'switch-to-prev-buffer)
(global-set-key (kbd "<M-S-down>") 'switch-to-next-buffer)

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key "\C-c\C-r" 'helm-recentf)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

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
