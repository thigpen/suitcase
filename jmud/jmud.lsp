;;; 
;;; 
;;; This file is used to load all jmud files ....
;;;
;;; 

(message "In ~/.emacs.jmud.")
(sit-for 1)

;(setq load-path 
;      (cons (expand-file-name "~HHughes/lib/languages/lisp") 
;      (cons (expand-file-name "~HHughes/lib/languages/lisp/jmud")
;load-path)))

(setq load-path 
      (cons (expand-file-name "~/suitcase/jmud/lib") 
load-path))

; ** for jmud
(autoload 'moo-code-mode "j-moo-code" "Major mode for editing MOO-code." t)
(setq auto-mode-alist (cons '("\\.moo$" . moo-code-mode) auto-mode-alist))
(global-set-key "\C-cm" 'mud)
(setq moo-use-@program t)
(setq moo-browser-worlds '(("LambdaMOO")))
(setq use-suppress-all-input t)
(setq moo-filter-hook
      (setq tinymud-filter-hook
	    '(mud-check-triggers mud-check-reconnect)))
(setq jmud-directory (expand-file-name "~HHughes/suitcase/jmud/"))
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
       "j-moo-code"
       "j-mcp"
       "j-text-mode"))
(defun j-mud-load () (interactive) (mapcar 'load-library j-mud-libraries-list))

(j-mud-load)

(global-set-key "\^c\^x" 'mud-get-text)

(autoload 'mud "mud" "Open a connection to a MUD." t)
(global-set-key "\^cm" 'mud)

(global-set-key "\C-cl" 'moo-mcp-init-connection)
