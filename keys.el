;; Custom key bindings
(global-set-key [f2] 'visit-ansi-term)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-+" 'word-count-mode)
(global-set-key (kbd "<C-tab>") 'yas/expand-from-trigger-key)

;; re-enable some functions
;;   C-x n d ... narrow to def
;;   C-x n n ... narrow to region
;;   C-x n p ... narrow to page
;;   C-x n w ... widen back
(put 'scroll-left 'disabled nil)
(put 'narrow-to-page 'disabled nil)
