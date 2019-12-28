(defcustom winny/before-theme-switch-hook nil
  "Hooks to run before changing themes in `winny/switch-theme'.")
(defcustom winny/after-theme-switch-hook nil
  "Hooks to run after changing themes in `winny/switch-theme'.")
(defcustom winny/default-theme 'wombat
  "Default theme to initialize after startup, or when calling
  `winny/default-theme'.")

;; XXX: 26.3 appears to cause problems with switching themes in emacs daemon
(defun winny/switch-theme (the-theme)
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
			     (mapcar 'symbol-name
				     (custom-available-themes))))))
  (lexical-let ((themes (if (listp the-theme) the-theme (list the-theme))))
    (run-hooks 'winny/before-theme-switch-hook)
    (mapc #'disable-theme custom-enabled-themes)
    (mapc #'(lambda (th) (load-theme th t)) themes)
    (run-hooks 'winny/after-theme-switch-hook)))

(defun winny/default-theme (&rest ignore)
  (interactive)
  (winny/switch-theme winny/default-theme))

;; Default theme
(add-hook 'after-init-hook 'winny/default-theme)
