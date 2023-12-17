;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom-set-*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open")))
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(bookmark-save-flag 1)
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(current-language-environment "UTF-8")
 '(custom-enabled-themes '(wheatgrass))
 '(custom-raised-buttons t)
 '(custom-safe-themes
   '("947190b4f17f78c39b0ab1ea95b1e6097cc9202d55c73a702395fc817f899393" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "6bc387a588201caf31151205e4e468f382ecc0b888bac98b2b525006f7cb3307" "d1cc05d755d5a21a31bced25bed40f85d8677e69c73ca365628ce8024827c9e3" "9fe1540491fcf692b8c639a3abacd32b29233bc4cb834a12a0fd1e01cbd0a128" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "f0d8af755039aa25cd0792ace9002ba885fd14ac8e8807388ab00ec84c9497d7" "c86f868347919095aa44d2a6129dd714cbcf8feaa88ba954f636295b14ceff8f" default))
 '(dash-docs-browser-func 'eww)
 '(debug-on-error nil)
 '(desktop-path '("~/.emacs.d/desktop"))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(dired-guess-shell-alist-user '(("\\.pdf$" "zathura") ("\\.djvu$" "zathura")))
 '(display-battery-mode t)
 '(display-buffer-alist
   '(("\\*Async Shell Command\\*.*"
      (display-buffer-no-window))
     ("*\\Man .*\\*"
      (display-buffer-same-window))))
 '(display-time-world-list
   '(("America/Los_Angeles" "Seattle")
     ("America/Chicago" "Chicago")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Asia/Hong_Kong" "Hong Kong")
     ("Asia/Tokyo" "Tokyo")))
 '(doc-view-resolution 144)
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(epresent-text-scale 200)
 '(fci-rule-color "#383838")
 '(global-hl-line-mode nil)
 '(gnus-logo-colors '("#4c8383" "#bababa") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(gnus-select-method
   '(nntp "news.easynews.com"
          (nntp-open-connection-function nntp-open-ssl-stream)
          (nntp-port-number 563)))
 '(helm-dash-browser-func 'eww)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(ibuffer-default-sorting-mode 'alphabetic)
 '(ibuffer-eliding-string "…")
 '(ibuffer-formats
   '((mark modified read-only " "
           (name 26 26 :left :elide)
           " "
           (size 9 -1 :right)
           " "
           (mode 16 16 :left :elide)
           " " filename-and-process)
     (mark " "
           (name 16 -1)
           " " filename)))
 '(ido-confirm-unique-completion t)
 '(indicate-buffer-boundaries t)
 '(inhibit-startup-screen t)
 '(irfc-assoc-mode t)
 '(irfc-directory "~/docs/RFC")
 '(irfc-download-base-url "https://www.ietf.org/rfc/")
 '(js-indent-level 2)
 '(magit-diff-use-overlays nil)
 '(magithub-clone-default-directory "~/code/")
 '(mediawiki-site-alist
   '(("Wikipedia" "https://en.wikipedia.org/w/" "username" "password" nil "Main Page")
     ("NetHackWiki" "https://nethackwiki.com/w/" "username" "password" nil "")))
 '(menu-bar-mode nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4"))
 '(org-agenda-files '("~/docs/orgs/todo.org"))
 '(org-hide-emphasis-markers nil)
 '(org-indent-mode-turns-on-hiding-stars t)
 '(org-latex-hyperref-template
   "\\hypersetup{
 pdfauthor={%a},
 pdftitle={%t},
 pdfkeywords={%k},
 pdfsubject={%d},
 pdfcreator={%c}, 
 pdflang={%L},
 colorlinks=true,
 filecolor=blue,
 urlcolor=blue,
 citecolor=red,
 linkcolor=red
}
")
 '(org-latex-image-default-width ".8\\linewidth")
 '(org-latex-inputenc-alist '(("\"utf8\"" . "\"utf8x\"")))
 '(package-selected-packages
   '(uwu-theme spacemacs-theme ob-async lispy nftables-mode org-contrib swift-mode all-the-icons-completion marginalia vertico terraform-mode whole-line-or-region yasnippet-snippets org-bullets borland-blue-theme ledger-mode ox-hugo sed-mode ssh-config-mode csproj-mode rg password-store which-key hl-todo counsel-etags counsel-tramp counsel-dash ivy-hydra jedi-direx expand-region vimish-fold helm-projectile helm-mode-manager kotlin-mode svelte-mode markdown web-server ansible prescient forge pass esup yafolding ebf cyberpunk-theme graphviz-dot-mode js-mode flycheck helpful flymake-shellcheck jade-mode paren-face god-mode ivy-prescient eink-theme nix-mode chronometer dashboard all-the-icons default-text-scale fast-scroll elfeed elfeed-org package-build mutt-mode meson-mode nhexl-mode basic-mode plantuml-mode counsel-projectile cyberpunk-2019-theme counsel captain org-trello mag-menu paredit-menu proceed editorconfig helm-dash leuven-theme libmpdel minions monokai-theme mpdel npm-mode olivetti org-tree-slide pdf-tools racket-mode smart-mode-line steam swiper tuareg use-package winum yaml-mode bind-key caml ivy rich-minority faceup tablist navigel dash-docs python-mode speed-type dired-sidebar protobuf-mode alect-themes rfc-mode sokoban ix sprunge webpaste inverse-acme-theme nofrils-acme-theme parchment-theme paredit material-theme dracula-theme ansi neotree flappymacs cloc clojure-mode ox-slack htmlize flymake-racket flycheck-haskell auto-virtualenvwrapper jedi chronos dictionary epresent 0blayout debbugs dockerfile-mode erlang shut-up epl git commander f dash s clippy ox-twbs epc json-mode mediawiki mode-line-bell rainbow-delimiters sicp mines rubik cask darkroom fill-column-indicator rainbow-mode gitignore-mode gitignore-templates fireplace wttrin dark-souls mingus libmpdee free-keys decide magithub buffer-move helm-systemd cmake-mode scala-mode sml-mode edit-server keychain-environment django-mode discover smex company-emacs-eclim company eclim gnu-apl-mode go-mode pkgbuild-mode benchmark-init transmission rust-mode pydoc-info pydoc pacmacs lua-mode image-dired+ image+ highlight-indentation haskell-mode gist enh-ruby-mode djvu dired+ csv-mode csharp-mode crosshairs conkeror-minor-mode coffee-mode browse-kill-ring ascii))
 '(pdf-view-midnight-colors '("#655370" . "#fbf8ef"))
 '(plantuml-default-exec-mode 'executable)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(rfc-mode-directory "/home/winston/docs/RFC/")
 '(safe-local-variable-values
   '((eval
      (lambda nil
        (when
            (and
             (buffer-file-name)
             (string=
              (file-name-extension
               (buffer-file-name))
              "rkt")
             (not
              (member
               (file-name-nondirectory
                (buffer-file-name))
               '("info.rkt" "main.rkt")))
             (not
              (equal major-mode 'basic-mode)))
          (message "dir-locals.el: Switching to BASIC mode...")
          (basic-mode))))
     (eval progn
           (org-indent-mode 1)
           (add-to-list 'winny/org-auto-insert-expiry-pattern-list
                        (expand-file-name default-directory)))
     (org-log-done . time)
     (eval web-mode-set-engine "django")
     (eval web-mode-set-engine "go")
     (eval add-to-list 'auto-mode-alist
           `(,(concat default-directory "[^/]+\\.\\(i\\|p\\|t\\)\\'")
             . abl-mode))
     (eval when
           (and
            (not
             (boundp 'winny/dirlocal-hack))
            (buffer-file-name)
            (string-match "\\.\\(i\\|p\\|t\\)$"
                          (buffer-file-name)))
           (setq-local winny/dirlocal-hack t)
           (abl-mode))
     (eval when
           (and
            (buffer-file-name)
            (string-match "\\.\\(i\\|p\\|t\\)$"
                          (buffer-file-name)))
           (abl-mode))
     (eval when
           (and
            (buffer-file-name)
            (string-match "\\.\\(i\\|p\\|t\\)$"
                          (buffer-file-name)))
           (fundamental-mode 1))
     (eval progn
           (auto-revert-mode 1)
           (org-indent-mode 1))
     (eval progn
           (auto-revert-mode 1)
           (org-indent-mode))
     (org-hide-emphasis-markers . t)
     (eval progn
           (use-local-map
            (copy-keymap text-mode-map))
           (local-set-key "<next>"
                          '(lambda nil
                             (interactive)
                             (forward-page)
                             (recenter-top-bottom 0)))
           (local-set-key "<prev>"
                          '(lambda nil
                             (interactive)
                             (backward-page)
                             (recenter-top-bottom 0))))
     (eval load
           (concat
            (projectile-project-root)
            "private/setup")
           nil t)
     (eval load
           (concat
            (projectile-project-root)
            "private/setup"))
     (eval ansible)
     (eval c-set-offset 'arglist-cont-nonempty
           '(c-lineup-gcc-asm-reg c-lineup-arglist))
     (eval c-set-offset 'arglist-close 0)
     (eval c-set-offset 'arglist-intro '++)
     (eval c-set-offset 'case-label 0)
     (eval c-set-offset 'statement-case-open 0)
     (eval c-set-offset 'substatement-open 0)
     (org-export-html-style . "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/stylesheet.css\" />")
     (eval when
           (fboundp 'rainbow-mode)
           (rainbow-mode 1))
     (elisp-lint-indent-specs
      (when-let . 1))
     (eval c-set-offset 'innamespace 0)
     (eval when
           (fboundp 'c-toggle-comment-style)
           (c-toggle-comment-style 1))))
 '(same-window-regexps '("\\*Man .*\\*"))
 '(symon-mode t)
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(tmm-completion-prompt nil)
 '(tmm-mid-prompt " → ")
 '(tmm-shortcut-style '(downcase upcase))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#c8805d801780")
     (60 . "#bec073400bc0")
     (80 . "#b58900")
     (100 . "#a5008e550000")
     (120 . "#9d0091000000")
     (140 . "#950093aa0000")
     (160 . "#8d0096550000")
     (180 . "#859900")
     (200 . "#66aa9baa32aa")
     (220 . "#57809d004c00")
     (240 . "#48559e556555")
     (260 . "#392a9faa7eaa")
     (280 . "#2aa198")
     (300 . "#28669833af33")
     (320 . "#279993ccbacc")
     (340 . "#26cc8f66c666")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(web-mode-code-indent-offset 2)
 '(web-mode-css-indent-offset 2)
 '(web-mode-markup-indent-offset 2)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))
 '(winum-mode t)
 '(world-clock-list
   '(("America/Los_Angeles" "Seattle")
     ("America/Chicago" "Chicago")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Asia/Hong_Kong" "Hong Kong")
     ("Asia/Tokyo" "Tokyo")))
 '(writeroom-extra-line-spacing 8)
 '(writeroom-fringes-outside-margins t)
 '(writeroom-fullscreen-effect 'maximized)
 '(writeroom-maximize-window nil)
 '(writeroom-mode-line t)
 '(writeroom-width 100)
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Info-quoted ((t (:inherit nil))))
 '(diredp-omit-file-name ((t (:inherit diredp-ignored-file-name))))
 '(hl-line ((t (:inherit highlight :background "#030"))))
 '(racket-xp-unused-face ((t nil)))
 '(sunrise-active-path-face ((t (:background "#04aa04" :foreground "yellow")))))
