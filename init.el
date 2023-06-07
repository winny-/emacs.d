;;; init --- my configuration
;;; Commentary:
;; This is a literate programming init.el.  As such all the code exists in an
;; org-mode document.  See ~/.emacs.d/configuration.org .
;;; Code:

(defun winny/reload-configuration ()
  "Reload your configuration.org"
  (interactive)
  (org-babel-load-file "~/.emacs.d/configuration.org"))
(winny/reload-configuration)

(provide 'init)
;;; init.el ends here
