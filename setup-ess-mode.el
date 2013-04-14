;; The files for ess-mode are installed in
;; /Applications/Emacs.app/Contents/Resources/site-lisp/ 
;; 

;; This activates the mode AFTER my ELPA/MELP/etc packages
;; load. Loading this first (the default without the hook) won't
;; work. This should run without error even if ESS is not installed on
;; your system.
(add-hook 'after-init-hook 
	  (lambda ()
	    (progn
			     
	      (if (require 'ess-site nil t)

		  ;; This prevents eval-region (C-c C-r) from printing commands in the R
		  ;; process buffer. By setting this to nil, there is a tremendous
		  ;; speedup for eval-region.
		  (setq ess-eval-visibly-p nil)

		;; The following line was added so that when using iESS to interact
		;; with R, the process buffer running R will always scroll 
		;; automatically when the output reaches the bottom of the window.
		(setq ess-comint-scroll-to-bottom-on-output t)

		;; Move the binding for 'ess-smart-underscore from Shift-minus to
		;; Control-= thereby restoring the usual way to enter an underscore
		;; character.  (princ "In all ESS buffers, 'C-=' will be bound to
		;; 'ess-smart-underscore. "). 
		;; (setq ess-S-assign-key (kbd "C-="))
		;; (ess-toggle-S-assign-key t)
		(ess-toggle-underscore nil)

		;; Some custom hooks
		(add-hook 'ess-mode-hook 
			  (lambda () 
			    (setq truncate-lines t)))

		(add-hook 'R-mode-hook 'auto-fill-mode)
		;; auto yasnippet creating for R mode
		;; (require 'r-autoyas)
		;; (add-hook 'ess-mode-hook 'r-autoyas-ess-activate)
))))

		;; Taken from Section 4.5 of the ESS manual:
(eval-after-load "comint" 
  '(progn 
     ;; The following makes the up/down keys behave like in the Matlab console window:
     (define-key comint-mode-map [up] 
       'comint-previous-matching-input-from-input) 
     (define-key comint-mode-map [down] 
       'comint-next-matching-input-from-input) 

     ;; Make C-left and A-left skip the R prompt at the beginning of line
     (define-key comint-mode-map [A-left] 'comint-bol)               
     (define-key comint-mode-map [C-left] 'comint-bol)               

     ;; also recommended for ESS use by the ESS manual:
     (setq comint-scroll-to-bottom-on-output 'others) 
     (setq comint-scroll-show-maximum-output t)
     ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above prompt
     (setq comint-scroll-to-bottom-on-input 'this) )) ; eval-after-load "comint"

(provide 'setup-ess-mode)
