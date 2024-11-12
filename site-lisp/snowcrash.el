(defvar snowcrash--refresh-rate "1 sec")

(defvar snowcrash--symbols '(?█ ? ))
;; (defvar snowcrash--symbols '(?. ? ))

(defvar snowcrash--buffer-name "*snowcrash*")

(defvar snowcrash--timer nil)

(defun snowcrash--draw ()
  "Draw snowcrash in the current buffer."
  (and-let* ((windows (get-buffer-window-list snowcrash--buffer-name))
             (lines (1- (cl-loop for window in windows
                                   minimize (window-total-height window 'floor))))
             (columns (1- (cl-loop for window in windows
                                       minimize (window-total-width window 'floor)))))
    (with-current-buffer (get-buffer snowcrash--buffer-name)
      (read-only-mode -1)
      (erase-buffer)
      (dotimes (i lines)
        (dotimes (j columns)
          (insert-char (nth (random (length snowcrash--symbols))
                            snowcrash--symbols)))
        (unless (= i (1- lines))
          (newline)))
      (goto-char (point-min))
      (read-only-mode 1))))

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
    (setq snowcrash--timer nil)
    (remove-hook 'quit-window-hook #'snowcrash--quit-window-hook)))

(define-derived-mode
  snowcrash-mode
  special-mode
  "Snowcrash"
  "Draw snowcrash noise.  Fun for the entire family!"
  (add-hook 'quit-window-hook #'snowcrash--quit-window-hook)
  (message "Type q to quit.")
  (snowcrash--tick))

(provide 'snowcrash)
