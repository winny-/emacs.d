;;; aps-mode --- Major mode for John Boyland's aps attribute grammar language.
;; 
;;; Commentary:
;;
;;; Code:

(defconst aps--keywords
  (regexp-opt '("and" "attribute" "begin" "case" "circular" "class"
                "collection" "constant" "constructor" "elscase" "else" "elsif"
                "end" "endif" "extends" "for" "function" "if" "in" "infix"
                "infixl" "infixr" "inherit" "input" "match" "module" "not" "on"
                "or" "pattern" "phylum" "pragma" "private" "procedure" "public"
                "remote" "signature" "then" "type" "var" "with")
              'symbols))

(defconst aps--constants
  "\\_<\\([0-9]+\\|false\\|null\\|true\\|nil\\)\\_>")

(defconst aps--highlights
  `(("\"[^\"]*\"" . font-lock-string-face)
    (,aps--keywords . font-lock-keyword-face)
    (,aps--constants . font-lock-constant-face)))

(defvar aps-mode-syntax-table
  (with-syntax-table (copy-syntax-table prog-mode-syntax-table)
    ;; main comment syntax: begins with "--", ends with "\n"
    (modify-syntax-entry ?- ". 12")
    (modify-syntax-entry ?\n ">")

    ;; "String"
    (modify-syntax-entry ?\" "\"")
    
    ;; _ is part of identifier
    (modify-syntax-entry ?_ "w")

    (syntax-table)))

(defun aps-font-lock-syntatic-face-function (state)
  "Return syntatic face given STATE."
  (if (nth 3 state)
    font-lock-string-face
    font-lock-comment-face))

(define-derived-mode aps-mode prog-mode "APS"
  "Major mode for editing APS grammars.

When started, runs `aps-mode-hook'."
  :syntax-table aps-mode-syntax-table
  (setq-local font-lock-defaults '(aps--highlights
                                   nil nil nil nil
                                   (font-lock-syntatic-face-function . aps-font-lock-syntatic-face-function)))
  (setq-local comment-use-syntax t)
  (setq-local comment-start "-- ")
;;  (setq-local comment-start-skip "---*[ \t]*")
  (setq-local comment-end ""))

;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.aps\\'" . aps-mode)))

(provide 'aps-mode)
;;; aps-mode.el ends here

