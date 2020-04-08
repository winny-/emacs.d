;;; init --- my configuration
;;; Commentary:
;;; Code:

;;; Customize

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; MELPA+ELPA package setup

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)


;;; Cask

(require 'cask "~/.cask/cask.el")
(cask-initialize "~/projects/emacs-dashboard/")

;;; Package requires / system loads

;; Uncomment for benchmarking emacs initialization.
;(require 'benchmark-init)
;(benchmark-init/activate)

;(add-to-list 'load-path "~/.emacs.d/custom/emacs-eclim")

;; Packaged by Gentoo
(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

;; Stuff not packaged elsewhere
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(load "scribble.el" nil t t)
(load "irfc.el" nil t t)
(load "dired+.el" nil t t)

;;; Built-in configuration

;; Dired ^ in customize (u is provided, but I always forget about it).
(require 'cus-edit)
(define-key custom-mode-map "^" 'Custom-goto-parent)

;; Better buffer list
(defalias 'list-buffers 'ibuffer)

(setq auto-window-vscroll nil)
;(setq source-directory "~/code/emacs-24.5")

;;(add-to-list 'display-buffer-alist
;;             `("\\*Async Shell Command\\*.*" (,#'display-buffer-no-window)))
;(load "webkit.el" nil t t)

;; Todo: dired+/dired
(setq-default frame-title-format '("(Emacs) "
                                   (:eval (if buffer-file-name
                                            (replace-regexp-in-string (regexp-quote (or (getenv "HOME") "")) "~" buffer-file-name)
                                            (buffer-name)))
                                   " [%m] { "
                                   (:eval (string-join (mapcar #'(lambda (w) (buffer-name (window-buffer w))) (window-list)) ", "))
                                   " }"))

(defvar my-backup-directory "~/.emacs.d/backup")
;; Sane backup file settings
(setq backup-directory-alist
      `((".*" . ,my-backup-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,my-backup-directory t)))

;; insert matching delimiters
(electric-pair-mode 1)

;; Disable tabs
(setq-default indent-tabs-mode nil)

(defun whitespace-hook ()
  "Hook to make trailing whitespace visible."
  (setq-local show-trailing-whitespace t))

;; Font
;; (let ((my-font (concat ;"Droid Sans Mono Slashed-"
;;                 "Go Mono-"
;;                                         ;(if (string-prefix-p "fightclub" system-name t) "10" "11")
;;                 "11"
;;                 )))
;;   (add-to-list 'default-frame-alist `(font . ,my-font))
;;   (set-face-attribute 'default t :font my-font))

(defun show-paren-local-mode (&optional arg)
  "Toggle visibility of matching parenthesis for the current buffer.
When ARG is positive or not a number, enable function
`show-paren-mode', else disable it."
  (interactive)
  (setq-local show-paren-mode
              (cond
               ((numberp arg) (> arg 0))
               ((not arg) (not show-paren-mode))
               (t t)))
  (when (called-interactively-p 'interactive)
    (message "show-paren-mode %s in current buffer." (if show-paren-mode "enabled" "disabled"))))

;; Enable word-wrap for org-mode.
(add-hook 'org-mode-hook (lambda ()
                           (setq word-wrap t)
                           (turn-on-auto-fill)))

;; c-mode
(add-hook 'c-mode-hook (lambda ()
                         (setq indent-tabs-mode t)
                         (c-set-style "bsd")))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'substatement-open 0)
            (if (assoc 'inexpr-class c-offsets-alist)
              (c-set-offset 'inexpr-class 0))))


(require 'seq)

;; javascript-mode
(setq js-indent-level 2)

(require 'org)

(add-hook 'irfc-mode-hook (lambda () (show-paren-local-mode -1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; free up some function keys
(global-unset-key (kbd "<f10>"))
(global-unset-key (kbd "<f3>"))
(global-unset-key (kbd "<f4>"))
;; move macro keys to f9-f10
(global-set-key (kbd "<f9>") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<f10>") 'kmacro-end-or-call-macro)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-x r v") 'view-register)
(define-key global-map (kbd "C-c P f") 'find-file-at-point)
(define-key global-map (kbd "C-c P u") 'browse-url-at-point)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(global-set-key (kbd "C-x K") 'bury-buffer)

(defun scroll-up-1 ()
  "Scroll up by 1 line."
  (interactive)
  (scroll-up 1))
(defun scroll-down-1 ()
  "Scroll down by 1 line."
  (interactive)
  (scroll-down 1))
(global-set-key (kbd "M-N") 'scroll-up-1)
(global-set-key (kbd "M-P") 'scroll-down-1)

(define-key org-mode-map (kbd "M-n") 'org-next-visible-heading)
(define-key org-mode-map (kbd "M-p") 'org-previous-visible-heading)

(require 'profiler)
(global-set-key (kbd "C-x M-p s") 'profiler-start)
(global-set-key (kbd "C-x M-p q") 'profiler-stop)
(global-set-key (kbd "C-x M-p r") 'profiler-report)

(require 'ox-latex)
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;;; Package configuration

;; Create multiple eww buffers
;; Via https://emacs.stackexchange.com/a/24477/9163
;; C-u M-x eww will force a new eww buffer
(defun modi/force-new-eww-buffer (orig-fun &rest args)
  "When prefix argument is used, a new eww buffer will be created,
regardless of whether the current buffer is in `eww-mode'."
  (if current-prefix-arg
    (with-temp-buffer
      (apply orig-fun args))
    (apply orig-fun args)))
(advice-add 'eww :around #'modi/force-new-eww-buffer)

(defun hide-fringes ()
  (set-window-fringes (selected-window) 0 0))

;; eclim-mode
;;(require 'eclim)
;;(global-eclim-mode)

;; company-mode
;;(require 'company)
;;(require 'company-emacs-eclim)
;;(company-emacs-eclim-setup)
;;(global-company-mode t)

;;(edit-server-start)

;; discover
;;(require 'discover)
;;(global-discover-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; smex
;;(require 'smex)

;; Can be omitted. This might cause a (minimal) delay when Smex is
;; auto-initialized on its first run.

;;(smex-initialize)

;;(global-set-key (kbd "M-x") 'smex)
;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "M-x") 'execute-extended-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(eval-after-load 'image+ '(imagex-global-sticky-mode 1))

(defun add-to-auto-mode-alist (mm extension &rest extensions)
  "Add major mode MM for EXTENSION and EXTENSIONS to the `auto-mode-alist'.
EXTENSION may also be a list."
  (let ((ls (if (listp extension)
              (append extension extensions)
              (cons extension extensions))))
    (dolist (ext ls)
      (add-to-list 'auto-mode-alist (cons (concat "\\." ext "\\'") mm)))
    auto-mode-alist))

(add-to-auto-mode-alist 'sh-mode "ebuild")

;; enh-ruby-mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-auto-mode-alist 'enh-ruby-mode "rb")
(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

;; auctex
(add-hook 'TeX-mode-hook (lambda ()
                           (setq word-wrap t)))

;; racket-mode
;;(add-hook 'racket-mode-hook (lambda ()
;;                              (put 'new 'racket-indent-function 'defun)))

(defun winny/setup-racket ()
  (put 'bit-string-case 'racket-indent-function 'defun))
(add-hook 'racket-mode-hook 'winny/setup-racket)

(require 'doc-view)
;;(setq doc-view-resolution 144)

;;(require 'transmission)
;;(define-key transmission-mode-map (kbd "A")
;;  (lambda ()
;;    (interactive)
;;    (transmission-add (read-string "Magnet URI: "))))

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (package-install 'use-package)
  (require 'use-package))

;;; File format support

(use-package python-mode
  :ensure t)

(use-package auto-virtualenvwrapper
  :ensure t
  :after python-mode
  :config
  (setq auto-virtualenvwrapper-verbose nil)
  ;; Activate on focus in
  (add-hook 'focus-in-hook #'auto-virtualenvwrapper-activate)
  ;; Activate on changing buffers
  (add-hook 'window-configuration-change-hook #'auto-virtualenvwrapper-activate)
  (add-hook 'python-mode-hook 'auto-virtualenvwrapper-activate))

(use-package jedi
  :ensure t
  :after python-mode
  :config
  (add-hook 'python-mode-hook 'jedi:setup))

(use-package jade-mode
  :ensure t)

(use-package csv-mode
  :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'")

(use-package nix-mode
  :ensure t)

(use-package pkgbuild-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package csharp-mode
  :ensure t)

(use-package coffee-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :hook (go-mode-hook
         .
         (lambda ()
           (setq tab-width 4)
           (setq indent-tabs-mode 1))))

(use-package scala-mode
  :ensure t)

(use-package graphviz-dot-mode
  :ensure t)


(use-package keychain-environment
  :ensure t
  :init
  (keychain-refresh-environment))

(use-package mode-line-bell
  :ensure t
  :init
  (mode-line-bell-mode 1))

;; XXX: this causes load-theme to hang.
;; XXX: Calling (winum-mode 1) causes similar hang.
;; (use-package winum
;;   :ensure t
;;   :bind (("C-x w m" . winum-select-window-0))
;;   :init
;;   (winum-mode 1))

(use-package god-mode
  :ensure t
  :bind (("<escape>" . god-local-mode))
  :init
  (defun my-update-cursor ()
    (setq cursor-type (if god-local-mode
                        'hbar
                        t)))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor))

(use-package paren-face
  :ensure t
  :config
  (setq paren-face-regexp (rx (any "()[]{}")))
  (add-to-list 'paren-face-modes 'racket-mode)
  (add-to-list 'paren-face-modes 'racket-reply-mode)
  (add-to-list 'paren-face-modes 'emacs-lisp-mode)
  (add-to-list 'paren-face-modes 'lisp-mode))

(use-package free-keys
  :ensure t
  :bind (("C-h Y" . free-keys)))

(use-package default-text-scale
  :ensure t
  :init
  (default-text-scale-mode 1))

(use-package paredit
  :ensure t
  :config
  (dolist (m '(emacs-lisp-mode-hook
     	       racket-mode-hook
     	       racket-repl-mode-hook))
    (add-hook m #'paredit-mode))
  (bind-keys :map paredit-mode-map
     	     ("{"   . paredit-open-curly)
     	     ("}"   . paredit-close-curly))
  (unless terminal-frame
    (bind-keys :map paredit-mode-map
     	       ("M-[" . paredit-wrap-square)
     	       ("M-{" . paredit-wrap-curly))))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-x M-c" . magit-clone)))

(use-package magithub
  :ensure t)

(use-package writeroom-mode
  :ensure t
  ;; :hook
  ;; (elfeed-show-mode . (lambda ()
  ;;                       (writeroom-mode 1)
  ;;                       ;; (debug)
  ;;                                       ;(elfeed-show-refresh)
  ;;                       ))
  )

(use-package elfeed
  :ensure t)

(use-package elfeed-org
  :ensure t
  :init
  (elfeed-org))

(use-package counsel
  :ensure t
  :init
  (counsel-mode 1))

(use-package counsel-projectile
  :ensure t
  :init
  (counsel-projectile-mode 1))

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle))
  :bind (:map neotree-mode-map
              ("^" . neotree-select-up-node)
              ("v" . neotree-select-down-node)))

(use-package sunrise
  :load-path "~/.emacs.d/sunrise-commander")

(use-package fast-scroll
  :ensure t
  :config
  ;; Keep `mode-line-format' the same. This addresses a problem with
  ;; disappearing winum mode-line indicies.
  (defun fast-scroll-default-mode-line ()
    mode-line-format)
  :init
  (fast-scroll-mode 1))

(use-package ivy
  :ensure t
  :config
  (defun winny/ivy-force-done ()
    "Complete ivy with entered text ignoring completions."
    (interactive)
    (ivy-alt-done t))
  (bind-keys :map ivy-minibuffer-map
             ("<C-return>" . winny/ivy-force-done)))

(use-package ivy-prescient
  :ensure t
  :init
  (ivy-prescient-mode))

(use-package prescient
  :ensure t
  :init
  (prescient-persist-mode 1))

(use-package projectile
  :ensure t
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  ;;(setq projectile-project-search-path '("~/projects" "~/code" "~/docs"))
  (setq projectile-project-search-path '("~/"))
  (projectile-mode 1))

(use-package editorconfig
  :ensure t
  :init
  (editorconfig-mode 1))

(use-package ox-twbs
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  ;; web-mode
  (add-to-auto-mode-alist 'web-mode "php" "phtml" "tpl" "[agj]sp" "as[cp]x"
                          "erb" "mustache" "d?html" "jsx")
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
      ad-do-it))
;; (add-hook 'web-mode-hook (lambda ()
;;                            (setq web-mode-markup-indent-offset 2)
;;                            (setq web-mode-css-indent-offset 2)
;;                            (setq web-mode-code-indent-offset 2)))
  )

;; I have a fork
;; (add-to-list 'load-path "~/projects/org-static-blog")
;; (load "org-static-blog.el" nil t t)
(use-package org-static-blog
  :ensure t
  :load-path "~/projects/org-static-blog/")

(use-package dashboard
  :ensure t
  :load-path "~/projects/emacs-dashboard/"
  :bind (:map dashboard-mode-map
              ("p" . dashboard-previous-line)
              ("n" . dashboard-next-line))
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-items '((projects . 5)
                          (recents . 5)
                          (bookmarks . 5))
        dashboard-item-shortcuts '((projects . "j")
                                   (recents . "r")
                                   (bookmarks . "m")
                                   (agenda . "a")
                                   (registers . "e"))
        dashboard-image-banner-max-height 50
        dashboard-image-banner-max-width 50)
  (dashboard-setup-startup-hook)
  (defun dashboard ()
    (interactive)
    (let ((buffer "*dashboard*"))
      (when (not (get-buffer buffer))
        (dashboard-insert-startupify-lists))
      (switch-to-buffer buffer))))

(use-package buffer-move
  :ensure t
  :bind (("C-x w p" . buf-move-up)
         ("C-x w n" . buf-move-down)
         ("C-x w b" . buf-move-left)
         ("C-x w f" . buf-move-right)))

(use-package swiper
  :ensure t
  :bind (("C-x M-s" . swiper)))

(use-package cyberpunk-theme
  :ensure t)

(use-package undo-tree
  :ensure t
  :init
  ;;(global-undo-tree-mode)
  )

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flymake
  :ensure t
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-h E" . flymake-show-diagnostics-buffer)))

(use-package flymake-shellcheck
  :ensure t
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (defun winny/sh-enable-shellcheck-hook ()
    (flymake-mode (if (member sh-shell '(bash sh ksh88)) 1 -1)))
  ;;(add-hook 'sh-set-shell-hook 'winny/sh-enable-shellcheck-hook)
  )

(use-package helpful
  :ensure t
  :bind (("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h f" . helpful-callable)))

(load "switch-theme.el" t t)
(setq winny/default-theme 'cyberpunk)
(use-package smart-mode-line
  :ensure t
  :config
  (add-hook 'winny/after-theme-switch-hook 'sml/setup t t))

;;(defun winum-enable () (winum-mode 1) (keyboard-quit))
;;(defun winum-disable () (winum-mode -1))
;;(add-hook 'winny/before-theme-switch-hook 'winum-disable t t)
;;(add-hook 'winny/after-theme-switch-hook 'winum-enable t t)


(global-set-key (kbd "C-x c") 'compile)
(global-set-key (kbd "C-x y") 'browse-kill-ring)

(mapc (lambda (m) (add-hook (intern (concat (symbol-name m) "-mode-hook"))
                            'whitespace-hook))
      '(c csv c++ python ruby enh-ruby js lisp web racket org TeX haskell makefile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; goto matching brace/paren
(global-set-key "\C-x," 'backward-sexp)
(global-set-key "\C-x." 'forward-sexp)

;; Nobody uses this and it messes with dwm.
(when window-system
  (global-unset-key (kbd "C-z")))

;;; Custom interactive functions

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buffer (buffer-list) (message "Refreshed open files"))
    (let ((fn (buffer-file-name buffer)))
      (when (and fn (not (buffer-modified-p buffer)))
        (if (file-exists-p fn)
          (progn
            (set-buffer buffer)
            (revert-buffer t t t))
          (message "Backing file `%s' no longer exists! Skipping." fn))))))

(defun kill-all-missing-buffers (no-ask)
  "Kill all buffers with missing files.

When prefix argument NO-ASK is non-nil, do not ask before killing
each buffer"
  (interactive "P")
  (dolist (buffer (buffer-list))
    (let ((fn (buffer-file-name buffer)))
      (when (and fn (not (file-exists-p fn)))
        (if no-ask
          (kill-buffer buffer)
          (kill-buffer-ask buffer))))))

(defun toggle-word-wrap ()
  "Toggle word wrap."
  (interactive)
  (message (format
            "Word wrap %s."
            (if (setq word-wrap (not word-wrap))
              "enabled"
              "disabled"))))

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun what-face (pos)
  "Describe the face under point.

Prefix argument POS should be a location it the buffer."
  (interactive "d")
  (let ((face (or (get-char-property (pos) 'read-face-name)
                  (get-char-property (pos) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(add-hook 'dired-mode-hook 'auto-revert-mode)

(eval-after-load 'dired
  '(progn
     (define-key dired-mode-map (kbd "C-c n") 'dired-create-file)
     (defun dired-create-file (file)
       "Create a file called FILE.
If FILE already exists, signal an error."
       (interactive
        (list (read-file-name "Create file: " (dired-current-directory))))
       (let* ((expanded (expand-file-name file))
              (try expanded)
              (dir (directory-file-name (file-name-directory expanded)))
              new)
         (if (file-exists-p expanded)
           (error "Cannot create file %s: file exists" expanded))
         ;; Find the topmost nonexistent parent dir (variable `new')
         (while (and try (not (file-exists-p try)) (not (equal new try)))
           (setq new try
                 try (directory-file-name (file-name-directory try))))
         (when (not (file-exists-p dir))
           (make-directory dir t))
         (write-region "" nil expanded t)
         (when new
           (dired-add-file new)
           (dired-move-to-filename))))))

(defun winny/raise-or-create-window-system-frame (display)
  "Raise an existing frame in the window system or create a new one.

DISPLAY is the X11 DISPLAY variable contents."
  (let ((frames (seq-filter #'(lambda (f) (frame-parameter f 'display)) (frame-list))))
    (if (null frames)
      (make-frame `((window-system . x)
                    (display . ,display)))
      (select-frame-set-input-focus (car frames)))))

;; C-u M-x eww will force a new eww buffer
;; (defun modi/force-new-eww-buffer (orig-fun &rest args)
;;   "When prefix argument is used, a new eww buffer will be created,
;; regardless of whether the current buffer is in `eww-mode'."
;;   (if current-prefix-arg
;;       (with-temp-buffer
;;         (apply orig-fun args))
;;     (apply orig-fun args)))
;; (advice-add 'eww :around #'modi/force-new-eww-buffer)

(defun eww-new ()
  (interactive)
  (let ((url (read-from-minibuffer "Enter URL or keywords: ")))
    (switch-to-buffer (generate-new-buffer "*eww*"))
    (eww-mode)
    (eww url)))

;; (with-eval-after-load 'flymake
;;   (flymake-racket-setup))
;; (add-hook 'racket-mode-hook #'flymake-mode)

(defun describe-current-theme ()
  "Describe the current theme, ignoring smart-mode-line themes."
  (interactive)
  (describe-theme
   (car
    (cl-remove-if (lambda (x)
                    (string-prefix-p "smart-mode-line" (symbol-name x)))
                  custom-enabled-themes))))

(defun show-trailing-whitespace (n)
  "Toggle the highlight of trailing whitespace for the current buffer.

  When N is nil, toggle the highlight setting.
  When N is non-negative, enable the highlight setting.
  When N is negative, disable the highlight setting."
  (interactive "P")
  (setq-local show-trailing-whitespace
              (cond
               ((eq n nil) (not show-trailing-whitespace))
               ((< n 0) nil)
               (t t)))
  (force-window-update)
  (message (if show-trailing-whitespace
             "Showing trailing whitespace."
             "Hiding trailing whitespace.")))

(global-set-key (kbd "C-x M-w") 'show-trailing-whitespace)

(defun winny/ensure-XDG_RUNTIME_DIR ()
  "Ensure XDG_RUNTIME_DIR is set.
Used by qutebrowser and other utilities."
  (let ((rd (getenv "XDG_RUNTIME_DIR")))
    (when (or (not rd) (string-empty-p rd))
      (setenv "XDG_RUNTIME_DIR" (format "/run/user/%d" (user-uid))))))

(add-hook 'after-init-hook #'winny/ensure-XDG_RUNTIME_DIR)

(load "portage.el" nil t t)

(defun remove-from-list (list-var element)
  "Remove ELEMENT from LIST-VAR."
  (setq list-var (delete element list-var)))

;; Make `if' a bit less stupid looking in elisp
(put 'if 'lisp-indent-function 'defun)

(display-battery-mode (if battery-status-function 1 -1))

(add-to-list 'Info-directory-list "~/docs/info" t)
(bind-key "y" #'Info-copy-current-node-name Info-mode-map)

(defun afs/org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address."
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
    (save-excursion
      (let ((remove (list (match-beginning 0) (match-end 0)))
            (description (if (match-end 3)
                           (org-match-string-no-properties 3)
                           (org-match-string-no-properties 1))))
        (apply 'delete-region remove)
        (insert description)))))

(load "shebang-change.el" nil t t)

(defun winny/change-prop-line-mode (mode &optional dont-change-mode)
  "Change the prop line's major MODE.
If DONT-CHANGE-MODE is not nil, dont change to that MODE first."
  (interactive "aMajor mode: \nP")
  (unless dont-change-mode
    (funcall-interactively mode))
  (delete-file-local-variable-prop-line 'mode)
  (let ((sans-mode (intern (replace-regexp-in-string "-mode$" "" (symbol-name mode)))))
    (add-file-local-variable-prop-line 'mode sans-mode nil)))

(defun winny/make-shebanged-file-executable ()
  "Make sure scripts with shebang are saved with expected permissions."
  (interactive)
  (when (and (save-excursion (goto-char (point-min)) (looking-at "#!"))
             (not (file-executable-p buffer-file-name)))
    (message "Making `%s' executable..." buffer-file-name)
    (executable-chmod)))

(add-hook 'after-save-hook 'winny/make-shebanged-file-executable)
(put 'narrow-to-region 'disabled nil)


(defun copy-buffer-file-name-as-kill (choice)
  "Copy the the buffer path to the `kill-ring'.
CHOICE can be `?f', `?d', or `?n' for full path, directory path,
or filename respectively.  Via
https://stackoverflow.com/a/18814469/2720026"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (kill-new new-kill-string))))
