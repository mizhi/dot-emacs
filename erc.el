;; Load up irc
(defun init-irc ()
  (interactive)
  (require 'erc)
  (require 'erc-fill)
  (require 'erc-goodies)
  (require 'erc-join)
  (require 'erc-match)
  (require 'erc-services)

  (defvar erc-insert-post-hook)

  (load (concat (getenv "HOME") "/.emacs.d/erc.pass"))

  (setq erc-auto-query 'window-noselect
        erc-autojoin-channels-alist '(("foonetic.net" "#xkcd")
                                      ("freenode.net" "#python" "#java" "#rubyonrails" "#django"))

        erc-nick-color-list '("white" "yellow" "red" "purple" "orange"
                              "magenta" "cyan" "brown" "blue")
        erc-echo-notices-in-minibuffer-flag t
        erc-hide-timestamps nil
        erc-current-nick-highlight-type 'nick-or-keyword
        erc-keywords '("\\bnsfw\\b")
        erc-log-insert-log-on-open t
        erc-log-channels t
        erc-log-channels-directory "~/.irclogs/"
        erc-log-write-after-send t
        erc-log-write-after-insert t
        erc-max-buffer-size 20000
        erc-nick-color-alist '()
        erc-prompt-for-nickserv-password nil
        erc-save-buffer-on-part nil
        erc-save-queries-on-quit nil
        erc-truncate-buffer-on-save t
        tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof"
                      "gnutls-cli -p %p %h"
                      "gnutls-cli -p %p %h --protocols ssl3"))
  (erc-autojoin-mode 1)

  ;; logging:
  (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
    (save-some-buffers t (lambda ()
                           (when (and (eq major-mode 'erc-mode)
                                      (not (null buffer-file-name)))))))

  (defun erc-get-color-for-nick (nick)
    "Gets a color for NICK. If NICK is in erc-nick-color-alist,
use that color, else hash the nick and use a random color from
the pool"
    (or (cdr (assoc nick erc-nick-color-alist))
        (nth
         (mod (string-to-number
               (substring (md5 nick) 0 6) 16)
              (length  erc-nick-color-list))
         erc-nick-color-list)))

  (defun erc-put-color-on-nick ()
    "Modifies the color of nicks according to erc-get-color-for-nick"
    (save-excursion
      (goto-char (point-min))
      (if (looking-at "<\\([^>]*\\)>")
          (let ((nick (match-string 1)))
            (put-text-property (match-beginning 1) (match-end 1)
                               'font-lock-face (list ':foreground (erc-get-color-for-nick nick)
                                                     ':weight 'bold))))))

  (define-minor-mode ncm-mode "" nil
    (:eval
     (let ((ops 0)
           (voices 0)
           (members 0))
       (maphash (lambda (key value)
                  (when (erc-channel-user-op-p key)
                    (setq ops (1+ ops)))
                  (when (erc-channel-user-voice-p key)
                    (setq voices (1+ voices)))
                  (setq members (1+ members)))
                erc-channel-users)
       (format " %S/%S/%S" ops voices members))))

  (defun connect-irc ()
    "Connect to IRC."
    (interactive)
    (erc-tls :server "irc.foonetic.net" :port 6697
             :nick "seggy" :full-name "segfaultzen" :password foonetic-pass)
    (erc-tls :server "irc.freenode.net" :port 6697
             :nick "seggy" :full-name "segfaultzen" :password freenode-pass))

  (add-hook 'erc-join-hook '(lambda ()
                              (ncm-mode)))

  (add-hook 'erc-insert-post-hook '(lambda ()
                                     (erc-truncate-buffer)
                                     (erc-save-buffer-in-logs)))

  (add-hook 'erc-mode-hook '(lambda ()
                              (when (not (featurep 'xemacs))
                                (set (make-variable-buffer-local
                                      'coding-system-for-write)
                                     'emacs-mule))))

  (add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)

  (erc-scrolltobottom-enable)
  (erc-match-mode t)
  (erc-services-mode t))
