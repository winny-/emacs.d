;;; ob-basic.el --- org-babel functions for Basic evaluation

;; Copyright (C) 2022 Winston Weinert

;; Author: winny
;; Keywords: literate programming, reproducible research
;; Homepage: https://github.com/winny-/emacs.d
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file is not intended to ever be loaded by org-babel, rather it is a
;; template for use in adding new language support to Org-babel. Good first
;; steps are to copy this file to a file named by the language you are adding,
;; and then use `query-replace' to replace all strings of "template" in this
;; file with the name of your new language.

;; After the `query-replace' step, it is recommended to load the file and
;; register it to org-babel either via the customize menu, or by evaluating the
;; line: (add-to-list 'org-babel-load-languages '(template . t)) where
;; `template' should have been replaced by the name of the language you are
;; implementing (note that this applies to all occurrences of 'template' in this
;; file).

;; After that continue by creating a simple code block that looks like e.g.
;;
;; #+begin_src basic

;; test

;; #+end_src

;; Finally you can use `edebug' to instrumentalize
;; `org-babel-expand-body:basic' and continue to evaluate the code block. You
;; try to add header keywords and change the body of the code block and
;; reevaluate the code block to observe how things get handled.

;;
;; If you have questions as to any of the portions of the file defined
;; below please look to existing language support for guidance.
;;
;; If you are planning on adding a language to org-babel we would ask
;; that if possible you fill out the FSF copyright assignment form
;; available at https://orgmode.org/request-assign-future.txt as this
;; will make it possible to include your language support in the core
;; of Org-mode, otherwise unassigned language support files can still
;; be included in the contrib/ directory of the Org-mode repository.


;;; Requirements:

;; Use this section to list the requirements of this language.  Most
;; languages will require that at least the language be installed on
;; the user's system, and the Emacs major mode relevant to the
;; language be installed as well.

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)
;; possibly require modes required for your language

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("basic" . "tmp"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:basic '())

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:basic' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:basic (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-basic nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-basic-var-to-basic (cdr pair))))
      vars "\n")
     "\n" body "\n")))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:basic (body params)
  "Execute a block of Basic code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing Basic source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the value of the session keyword is not the
         ;; string `none'
         (session nil)
         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         (result-params (assq :result-params processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (assq :result-type processed-params))
         ;; expand the body with `org-babel-expand-body:basic'
         (full-body (org-babel-expand-body:basic
                     body params processed-params)))
    (org-babel-eval "racket -l tinybasic -- --run"
		    full-body)
    ;; actually execute the source-code block either in a session or
    ;; possibly by dropping it to a temporary file and evaluating the
    ;; file.
    ;;
    ;; for session based evaluation the functions defined in
    ;; `org-babel-comint' will probably be helpful.
    ;;
    ;; for external evaluation the functions defined in
    ;; `org-babel-eval' will probably be helpful.
    ;;
    ;; when forming a shell command, or a fragment of code in some
    ;; other language, please preprocess any file names involved with
    ;; the function `org-babel-process-file-name'. (See the way that
    ;; function is used in the language files)
    ))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:basic (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-basic-var-to-basic (var)
  "Convert an elisp var into a string of basic source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-basic-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-basic-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-basic)
;;; ob-basic.el ends here
