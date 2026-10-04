#!/usr/bin/env -S emacs --script

(require 'ox-publish)
;; (require 'htmlize)

(let* ((root (expand-file-name
              (concat (file-name-directory load-file-name)
                      "/..")))
       (public (expand-file-name (concat root "/public")))
       (expected-files '("misc" ".git" "configuration.org"))
       (org-publish-project-alist
        '(("org-docs" . (:base-directory "."
                                       :publishing-directory "./public"
                                       :publishing-function org-html-publish-to-html
                                       :base-extension "org"
                                       :section-numbers nil
                                       :htmlized-source t
                                       :html-head "<link rel=\"stylesheet\" href=\"misc/style.css\" type=\"text/css\">"))
          ("org-static"
           :base-directory "."
           :publishing-directory "./public"
           :base-extension "png\\|jpg\\|webm\\|gif\\|css"
           :include ("LICENSE")
           :exclude "\\(elpa\\|image-dired\\|site-packages\\|straight\\)/"
           :recursive t
           :publishing-function org-publish-attachment)
          ("emacs.d"
           :components ("org-docs" "org-static")))))
  (setq default-directory root)
  (cl-loop for file in expected-files
           do (unless (file-exists-p file)
                (error
                 (format "publish: sanity check failed.  Expected file \"%s\" in root."
                         file))))
  (delete-directory public t)
  (org-publish-project "emacs.d" t)
  (message "published at %s" public))
