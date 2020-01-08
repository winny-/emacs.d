(defun winny/add-shebang-check-locally ()
  (interactive)
  (when (and (let ((f (buffer-file-name))) (and f (not (string-equal (file-name-nondirectory f) "COMMIT_EDITMSG")))))
    (add-hook 'after-change-functions 'winny/check-if-shebang-changed-hook t t)
    (add-hook 'before-change-functions 'winny/watch-for-shebang-removal-hook t t)))

(defvar-local winny/removed-shebang nil)

(defun winny/watch-for-shebang-removal-hook (beginning end)
  (lexical-let ((a (<= beginning (+ 1 (point-min))))
                (b (save-excursion (goto-char (point-min)) (looking-at "#!"))))
;    (message "%s %s" a b)
    (setq-local winny/removed-shebang (and a b))))

(defun winny/check-if-shebang-changed-hook (beginning end region)
  "Hook for `after-change-functions' that checks if the shebang has changed, and if a different mode should be used.

Concern: since this is called every time an edit is made, it
should be extremely fast. Presently it appears to be a little bit
slower than electric-pair-mode's hook.
"
;  (message "%s" winny/removed-shebang)
  (when (and (not (minibufferp)) ; omit minibuffer
             buffer-file-name ; omit indirect buffers
             (<= beginning (save-excursion (goto-char (point-min)) (point-at-eol))) ; first line only
             (or (save-excursion (goto-char (point-min)) (looking-at "#!")) ; Only if first two chars look like a shebang.
                 winny/removed-shebang))
    (set-auto-mode t)))

(add-hook 'find-file-hook 'winny/add-shebang-check-locally)
(add-hook 'after-change-major-mode-hook 'winny/add-shebang-check-locally)
