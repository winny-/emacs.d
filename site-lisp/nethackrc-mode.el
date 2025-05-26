;;; nethackrc-mode --- Support glue for NetHack nethackrc configuration files.
;;; Commentary: A crappy mode derived from `conf-unison-mode' to (poorly)
;;              syntax-highlight nethackrc-mode files.
;;; Code:

(require 'conf-mode)

(defvar conf-nethackrc-mode-options
  (mapcar 'symbol-name
          '(OPTIONS                     ; https://nethackwiki.com/wiki/Options
            MSGTYPE                     ; https://nethackwiki.com/wiki/MSGTYPE
            MENUCOLOR                   ; https://nethackwiki.com/wiki/Menucolors
            SOUNDDIR SOUND              ; https://nethackwiki.com/wiki/User_sounds
            AUTOPICKUP_EXCEPTION        ; https://nethackwiki.com/wiki/Autopickup
            BOULDER                     ; https://nethackwiki.com/wiki/Options#boulder
            SYMBOLS
            AUTOCOMPLETE                ; https://nethackwiki.com/wiki/Options#AUTOCOMPLETE
            BIND                        ; https://nethackwiki.com/wiki/Options#BIND
            CHOOSE                      ; https://nethackwiki.com/wiki/Options#CHOOSE
            WIZKIT                      ; https://nethackwiki.com/wiki/Options#wizkit
            WARNINGS                    ; https://nethackwiki.com/wiki/Custom_map_symbols_(3.4.3)
            GRAPHICS DUNGEON TRAPS EFFECTS MONSTERS ; historic map symbols
            )))

(defvar conf-nethackrc-mode-font-lock-keywords
  `((,(concat "^[ \t]*" (regexp-opt  conf-nethackrc-mode-options 'symbols)) . font-lock-variable-name-face)))

(define-derived-mode nethackrc-mode conf-unix-mode "NetHack rc"
  "Conf Mode starter for Portage files"
  (conf-mode-initialize "#" 'conf-nethackrc-mode-font-lock-keywords)
  (conf-quote-normal 1)
  (conf-quote-normal 2)
  ;; (let ((table (copy-syntax-table (syntax-table))))
  ;;   (modify-syntax-entry ?: "." table)
  ;;   (set-syntax-table table)
  ;;   (font-lock-flush))
  ;;(setq-local syntax-propertize-function conf-nethackrc-mode-syntax-propertize)
  )

(add-to-list 'auto-mode-alist '("/.nethackrc\\\'" . conf-nethackrc-mode))
(add-to-list 'auto-mode-alist '("/defaults.nh\\\'" . conf-nethackrc-mode))
(add-to-list 'auto-mode-alist '("/NetHack Defaults\\\'" . conf-nethackrc-mode))
(add-to-list 'auto-mode-alist '("/NetHack.cnf\\\'" . conf-nethackrc-mode))


(provide 'nethackrc-mode)
;;; nethackrc-mode.el ends here
