(defvar snowcrash--refresh-rate "1 sec")

(defvar snowcrash--symbols '(?█ ? ))
;; (defvar snowcrash--symbols '(?. ? ))

(defvar snowcrash--buffer-name "*snowcrash*")

(defvar snowcrash--timer nil)

(defun snowcrash--draw ()
  "Draw snowcrash in the current buffer."
  (with-current-buffer (get-buffer snowcrash--buffer-name)
    (read-only-mode -1)
    (erase-buffer)
    (dotimes (lines (1- (window-total-height nil 'floor)))
      (dotimes (columns (1- (window-total-width nil 'floor)))
        (insert-char (nth (random (length snowcrash--symbols))
                          snowcrash--symbols)))
      (unless (= lines (- (window-total-height nil t) 2))
        (newline)))
    (goto-char (point-min))
    (read-only-mode 1)))

(defun snowcrash--tick ()
  (snowcrash--draw)
  (setq snowcrash--timer
        (run-with-timer snowcrash--refresh-rate t #'snowcrash--tick)))

(defun snowcrash ()
  (interactive)
  (switch-to-buffer (get-buffer-create snowcrash--buffer-name))
  (snowcrash-mode))

(defun snowcrash--quit-window-hook ()
  (when (and (eq major-mode 'snowcrash-mode)
             snowcrash--timer)
    (cancel-timer snowcrash--timer)
    (setq snowcrash--timer nil)))

(define-derived-mode
  snowcrash-mode
  special-mode
  "Snowcrash"
  "Draw snowcrash noise.  Fun for the entire family!"
  (add-hook 'quit-window-hook #'snowcrash--quit-window-hook)
  (message "Type q to quit.")
  (snowcrash--tick))

(provide 'snowcrash)
