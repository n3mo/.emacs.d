;; Customizations unique to running Emacs on OS X

;; Change command to meta, and ignore option so that special
;; characters can be entered with option
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)

;; Emacs for mac os x doesn't find my bash PATH, so I've specified it
;; here manually. There's probably a more elegent way of doing this. I
;; noticed Magnars (Emacs Rocks!) does something in his setup file
;; mac.el
(setenv "PATH"
	(concat
	 "/Users/nemo/bin" ":"
	 "/opt/subversion/bin" ":"
	 "/sw/bin" ":"
	 "/sw/sbin" ":"
	 "/bin" ":"
	 "/sbin" ":" 
	 "/usr/bin" ":" 
	 "/usr/sbin" ":"
	 "/usr/local/bin" ":"
	 "/usr/texbin"
	 ))

;; Specifying the path to the ghostscript binary seems nessary to get
;; doc-view to work. This problem (and other "Emacs for Mac OS X" path
;; problems) can also be fixed by launchin Emacs.app from the
;; terminal, rather than from the doc or finder. Launching emacs.app
;; in that way ensures my full paths are loaded properly.
(setq exec-path (append exec-path '("/sw/bin")))

;; This adds support for using OS X's address book database in
;; GNUS. This allows you to start typing someone's name in an a
;; message header and then press C-c TAB to complete their email
;; address. It relied on the command line tool "contacts" which I
;; installed to provide command line interaction the address book. 
;; I have since adopted use of the Insidious Big Brother Database, so
;; I am unlikely to need this slower option anymore. However, my OS X
;; address book has different contacts and I could conceivably need
;; them down the road, so this hack remains open
(require 'external-abook)
(setq external-abook-command "contacts -lSf '%%e\t\"%%n\"' '%s'")
(eval-after-load "message" 
  '(progn 
     (add-to-list 'message-mode-hook 
		  '(lambda ()
		     (define-key message-mode-map "\C-c\t"
		       'external-abook-try-expand)))))

;; Move to trash when deleting stuff
(setq delete-by-moving-to-trash t
      trash-directory "~/.Trash/emacs")

(provide 'mac)
