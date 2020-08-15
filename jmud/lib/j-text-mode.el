;; Minor mode for dealing with jtext. It's not general since it assumes that
;; it's being run in a mud buffer under jmud. Oh well.
;;
;;
;; To use jtext-mode.el, you must be running Emacs 19. You also need to be
;; running jmud with a modified j-mud-get.el. After you load load jmud 
;; (with the patch provided with jtext-mode), load this file, jtext.el.
;; After you load it, type M-x jtext-mode (uh, in a moo interaction buffer).
;; That's all there is to it.
;;
;; 
;; If you have any questions or comments, feel free to send e-mail to
;; tapia@hydra.unm.edu. 
;;
;;
;; Last modified June 1992: (Ron) 
;;                          (Erik) didn't really modify, but support for
;;                                 jtext-mode is now in j-mud-get.el

(defvar jtext-tag-actions '((title . ignore) ;; we don't do headers (yet)
			    (string . insert-before-markers)
			    (text . insert-before-markers)
			    (header . jtext-insert-header)
			    (hgroup . jtext-insert-hgroup)
			    (paragraph . jtext-insert-paragraph)
			    (link . jtext-insert-link)
			    )
  "List of jtext tags and functions to pass control to.")

(defun jtext-tag-class (form)
  "To what class does the given form belong?"
  (if (stringp form)
      'string
    (car form)))

(defun jtext-tag-values (form)
  "What values are associated with the tag in the given form?"
  (if (stringp form)
      (list form)
    (cdr form)))

(defun jtext-insert-vbox (form)
  "Given a vbox, insert it.
The vbox should be a string or a list, and can be obtained by passing
jtext-tagged text to the read function."
  (let ((a (assoc (jtext-tag-class form) jtext-tag-actions)))
    (if a
	(apply (cdr a) (jtext-tag-values form))
      (message "Form not known")
      (insert-before-markers (prin1-to-string form))))
  (insert-before-markers "\n"))

(defvar jtext-header-actions '(jtext-header-action-1
				      jtext-header-action-2
				      jtext-header-action-3
				      jtext-header-action-4
				      jtext-header-action-5
				      jtext-header-action-6)
  "List of actions to take when given header of various levels.")

(defvar jtext-header-faces '(jtext-header-1
			     jtext-header-2
			     jtext-header-3
			     jtext-header-4
			     jtext-header-5
			     jtext-header-6))

;;(mapcar 'make-face jtext-header-faces)
;;(mapcar (lambda (face) 
;;	  (set-face-underline-p face t))
;;	jtext-header-faces)


(defun jtext-header-action-1 (start end)
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-1)))
(defun jtext-header-action-2 (start end )
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-2)))
(defun jtext-header-action-3 (start end )
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-3)))
(defun jtext-header-action-4 (start end )
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-4)))
(defun jtext-header-action-5 (start end )
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-5)))
(defun jtext-header-action-6 (start end )
  (add-text-properties start end (list 'jtext-start start 'end end))
  (add-text-properties start end  '(face jtext-header-6)))




(defun jtext-insert-header (level &rest forms)
  (let ((start (point)))
    (mapcar 'jtext-insert-hbox forms)
    (funcall (nth (1- level) jtext-header-actions)
	     start (point))))

(defun jtext-insert-hbox (form)
  "Insert the given form, which should be an hbox."
  (let ((a (assoc (jtext-tag-class form) jtext-tag-actions)))
    (if a
	(apply (cdr a) (jtext-tag-values form))
      (message "Form not known")
      (insert-before-markers (prin1-to-string form)))))

(defun jtext-insert-paragraph (&rest forms)
  (mapcar 'jtext-insert-hbox forms))
(defun jtext-insert-hgroup (&rest forms)
  (mapcar 'jtext-insert-hbox forms))

(defun jtext-insert-link (address link-text)
  (let ((start (point)))
    (jtext-insert-hbox link-text)
    (let ((point (point)))
;;;      (set-extent-attribute jextent 'highlight)
      (add-text-properties start point (list 'jtext-start start 'jtext-end point))
      (add-text-properties start point '(face bold-italic))
      (add-text-properties start point (list 'address address)))))
      

(defvar jtext-mode nil "Records whether jtext-mode is on or off.")
(make-variable-buffer-local 'jtext-mode)
(defvar jtext-mode-saved-local-keymap nil "Keymap in place before entering jtext-mode.")
(make-variable-buffer-local 'jtext-mode-saved-local-keymap)

(defun jtext-mode (&optional arg)
  (interactive)
  (setq jtext-mode
	(if (null arg) (not jtext-mode)
	  (> (prefix-numeric-value arg) 0)))
  (if jtext-mode
      (progn
	(setq jtext-mode-saved-local-keymap (current-local-map))
       (define-key (current-local-map) [mouse-1] 'jtext-select-link)
	(define-key (current-local-map) [C-down-mouse-3]  'kill-region)
	(or (assq 'jtext-mode minor-mode-alist)
	    (setq minor-mode-alist
		  (cons '(jtext-mode " jtext") minor-mode-alist)))
	(use-local-map jtext-mode-saved-local-keymap))))

(defun jtext-indicated-link (event)
  (end-of-buffer)
  (save-window-excursion
    (save-excursion
      (let* ((position (event-start event))
	     (buffer (window-buffer (posn-window position)))
	     (p (posn-point position))
	     (jtext-address (get-text-property p 'address)) 
	     (text (and jtext-address
			(save-excursion
			  (set-buffer buffer)
			  (buffer-substring
			   (get-text-property p 'jtext-start)
			   (get-text-property p 'jtext-end)))))
	     (case-fold-search t)
	     i)
	(if jtext-address
	    (let* ((address jtext-address)
		   (to-send (concat "#$#jtext-pick address-type: "
				    (symbol-name (car address))
				    " args: \"")))
	      (mapcar (function
		       (lambda (pair)
			 (let ((s (prin1-to-string
				   (prin1-to-string
				    (car (cdr pair))))))
			   (setq to-send (concat to-send (car pair) ": "
						 (substring s 1
							    (1- (length s)))
						 " ")))))
		      (nth 1  address))
	      (mud-send-here (concat to-send "\""))
	      t))))))

(defun jtext-select-link (event)
  (interactive "e")
  ;(mouse-set-point event)
  (or (jtext-indicated-link event)
      (mouse-set-point event)))

