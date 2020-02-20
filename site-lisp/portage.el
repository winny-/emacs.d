;;; portage --- Support glue for portage config files.
;;; Commentary: A crappy mode derived from `conf-mode' to (poorly) syntax-highlight portage files.
;;; Code:

(require 'diff-mode)
(require 'conf-mode)

(defvar conf-portage-font-lock-keywords
  '(;; package atom
    ("^[A-Za-z0-9/_:=><.*-~]+" 0 'font-lock-function-name-face)
    ;; -useflag and **
    ("[ \t]+\\(-[~0-9A-Za-z_-]+\\|\\*\\*\\)" 1 'diff-removed)
    ;; useflag +useflag
    ("[ \t]+\\([0-9A-Za-z_.+][0-9A-Za-z_.-]+\\)" 1 'diff-added)
    ;; ~keyword
    ("[ \t]+\\(~[0-9A-Za-z_.-]+\\)" 1 'font-lock-string-face)))

(define-derived-mode conf-portage-mode conf-unix-mode "Conf[Portage]"
  "Conf Mode starter for Portage files"
  (conf-mode-initialize "#" 'conf-portage-font-lock-keywords))

(add-to-list 'auto-mode-alist
             '("/etc/portage/package\\.\\(accept_.*\\|use.*\\|unmask\\|mask\\|env\\)"
               . conf-portage-mode))
(add-to-list 'auto-mode-alist '("/etc/conf\\.d/" . sh-mode))
(add-to-list 'auto-mode-alist '("/etc/\\(portage/\\)?make.conf" . sh-mode))

(provide 'portage)
;;; portage.el ends here
