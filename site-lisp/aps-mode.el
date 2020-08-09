;;; aps-mode --- Major mode for John Boyland's aps attribute grammar language.
;; 
;;; Commentary:
;;
;; This code is covered under Unlicense:
;;
;; This is free and unencumbered software released into the public
;; domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a
;; compiled binary, for any purpose, commercial or non-commercial, and
;; by any means.
;;
;; In jurisdictions that recognize copyright laws, the author or
;; authors of this software dedicate any and all copyright interest in
;; the software to the public domain. We make this dedication for the
;; benefit of the public at large and to the detriment of our heirs
;; and successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to
;; this software under copyright law.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
;; CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;;
;; For more information, please refer to <http://unlicense.org/>
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

