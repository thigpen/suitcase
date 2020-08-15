(setq load-path (cons
		 (expand-file-name "~/emacs/jmud")
		 load-path))
(autoload 'moo-code-mode "j-moo-code" "Major mode for editing MOO-code." t)
(setq auto-mode-alist (cons '("\\.moo$" . moo-code-mode) auto-mode-alist))
(global-set-key "\C-cm" 'mud)
(setq moo-use-@program t)
(setq moo-browser-worlds '(("LambdaMOO")))
(setq use-suppress-all-input t)
(setq moo-filter-hook
      (setq tinymud-filter-hook
	    '(mud-check-triggers mud-check-reconnect)))
(setq jmud-directory (expand-file-name "~/emacs/jmud/"))
(setq j-mud-libraries-list
      (list
       "j-mud-worlds"
       "j-mud"
       "j-mud-get"
       "j-mud-macros"
       "j-mud-history"
       "j-mud-upload"
       "prefix"
       "j-boom-tree"
       "j-boom-obj"
       "j-boom"
       "j-lp"
       "j-tiny"
       "j-moo"
       "j-moo-code"))
(defun j-mud-load () (interactive) (mapcar 'load-library j-mud-libraries-list))
