;;; unison --- Support glue for unison profiles.
;;; Commentary: A crappy mode derived from `conf-mode' to (poorly) syntax-highlight unison files.
;;; Code:

(require 'conf-mode)

(defvar conf-unison-mode-constants
  (mapcar 'symbol-name
          '(CURRENT1 CURRENT2 CURRENTARCH CURRENTARCHOPT NEW BELOWPATH PATH REGEX NAME NEW1 NEW2 true false)))

(defvar conf-unison-mode-options
  (mapcar 'symbol-name
          '(auto batch doc fat group ignore ignorenot nocreation nodeletion noupdate
            owner path perms root silent terse testserver times version addprefsto
            addversionno atomic backup backupcurr backupcurrnot backupdir backuploc
            backupnot backupprefix backups backupsuffix clientHostName confirmbigdel
            confirmmerge contactquietly copymax copyonconflict copyprog copyprogrest
            copyquoterem copythreshold debug diff dontchmod dumbtty fastcheck
            fastercheckUNSAFE follow force forcepartial halfduplex height host
            ignorearchives ignorecase ignoreinodenumbers ignorelocks immutable
            immutablenot key killserver label links log logfile maxbackups maxerrors
            maxsizethreshold maxthreads merge mountpoint nocreationpartial
            nodeletionpartial noupdatepartial numericids prefer preferpartial repeat
            retry rootalias rsrc rsync selftest servercmd showarchive socket sortbysize
            sortfirst sortlast sortnewfirst sshargs sshcmd stream ui unicode watch
            xferbycopying)))

(defvar conf-unison-mode-font-lock-keywords
  `((,(concat "^[ \t]*" (regexp-opt conf-unison-mode-options 'symbols)) . font-lock-variable-name-face)
    (,(regexp-opt conf-unison-mode-constants 'symbols) . font-lock-constant-face)
    ("{[^}]*}" . font-lock-string-face)
    ("=[ \t]*\\(?:Path\\|Regex\\|Name\\).*\\(->\\)" (1 font-lock-preprocessor-face))))

;; (defvar conf-unison-mode-syntax-propertize
;;   (syntax-propertize-rules
;;    ("}" (0 (unless (nth 8 (save-excursion (syntax-ppss (match-beginning 0))))
;;              (string-to-syntax "|"))))
;;    ("{" (0 (when (eq t (nth 3 (save-excursion (syntax-ppss (match-beginning 0)))))
;;              (string-to-syntax "|"))))))

(define-derived-mode conf-unison-mode conf-unix-mode "Conf[Unison]"
  "Conf Mode starter for Portage files"
  (conf-mode-initialize "#" 'conf-unison-mode-font-lock-keywords)
  (conf-quote-normal 1)
  (conf-quote-normal 2)
  ;;(setq-local syntax-propertize-function conf-unison-mode-syntax-propertize)
  )

(add-to-list 'auto-mode-alist '("\\.prf\\'" . conf-unison-mode))

(provide 'unison)
;;; unison.el ends here
