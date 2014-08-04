(defun first-existing-file (file-list)
  (if file-list
      (if (file-exists-p (car file-list))
          (car file-list)
        (first-existing-file (cdr file-list)))
    nil))

(defun untabify-before-save ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "[ \t]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0)))
    (goto-char (point-min))
    (if (search-forward "\t" nil t)
        (untabify (1- (point)) (point-max))))
  nil)

(defun visit-ansi-term ()
  (interactive)
  (let ((existing-term (get-buffer "*ansi-term*")))
    (if existing-term
        (switch-to-buffer "*ansi-term*")
      (ansi-term explicit-shell-file-name))))
