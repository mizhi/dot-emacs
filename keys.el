;; Magit bindings
(define-prefix-command 'magit-map)
(global-set-key (kbd "C-c g") 'magit-map)
(define-key magit-map (kbd "s") 'magit-status)
(define-key magit-map (kbd "b") 'magit-blame-mode)

;; projectile bindings
(define-prefix-command 'projectile-map)
(global-set-key (kbd "C-c p") 'projectile-map)
(define-key projectile-map (kbd "f") 'projectile-find-file)
(define-key projectile-map (kbd "s") 'projectile-switch-project)

;; Some convenience things
(global-set-key [f2] 'visit-ansi-term)
(global-set-key [f8] 'toggle-frame-fullscreen)

(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-+" 'word-count-mode)
(global-set-key (kbd "<C-tab>") 'yas/expand-from-trigger-key)

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
