;;; funcs.el --- Some useful functions for my emacs init.

;;; Commentary:

;;; Code:


(defun load-if-exists (filename)
  "Only load a FILENAME if it's in the filesystem."
  (if (file-exists-p filename)
      (load filename)))

(defun load-init-el (filename)
  "Load FILENAME for initialization from `emacs.d`."
  (let ((full-filename (concat user-emacs-directory filename)))
    (load-if-exists full-filename)))

(defun first-existing-file (file-list)
  "Return the first file from FILE-LIST that exists on the file system."
  (if file-list
      (if (file-exists-p (car file-list))
          (car file-list)
        (first-existing-file (cdr file-list)))
    nil))

(defun untabify-before-save ()
  "Remove tabs from file before saving."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  nil)

(defun visit-ansi-term ()
  "Open up a new 'ansi-term' or switch to the buffer if already open."
  (interactive)
  (let ((existing-term (get-buffer "*ansi-term*")))
    (if existing-term
        (switch-to-buffer "*ansi-term*")
      (ansi-term explicit-shell-file-name))))

(defun toggle-fullscreen ()
  "Toggle full screen on X11."
  (interactive)
  (cond
   ((eq window-system 'x)
    (set-frame-parameter nil 'fullscreen
                         (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
   ((eq window-system 'ns)
    (toggle-frame-fullscreen))))

;;; funcs.el ends here
