;;; is-wsl --  Detect WSL
;;; Commentary:
;; See https://www.reddit.com/r/bashonubuntuonwindows/comments/752kvy/how_does_a_bash_script_know_its_running_in_wsl/
;;; Code:

(defconst is-wsl
  (with-temp-buffer
    (condition-case nil
        (progn
          (insert-file-contents "/proc/version")
          (goto-char (point-min))
          (and (search-forward "Microsoft") t))
      (error nil)))
  "Is Emacs running under WSL?")

(provide 'is-wsl)
;;; is-wsl.el ends here
