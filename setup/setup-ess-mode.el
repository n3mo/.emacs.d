;; I no longer install ESS manually, opting instead to install from
;; the MELPA repository using Emacs package.el.  My setup file
;; activates the mode AFTER my ELPA/MELP/etc packages load, otherwise
;; ess-site won't be recognized. This should run without error even if
;; ESS is not installed on your system.
(add-hook 'after-init-hook 
	  (lambda ()
	    (if (require 'ess-site nil t)
		(progn

		  ;; This prevents eval-region (C-c C-r) and similar
		  ;; commands from printing in the R process
		  ;; buffer. By setting this to nil, there is a
		  ;; tremendous speedup for eval-region (results are
		  ;; still printed, just not the invoking code)
		  (setq ess-eval-visibly-p nil)

		  ;; The following line was added so that when using
		  ;; iESS to interact with R, the process buffer
		  ;; running R will always scroll automatically when
		  ;; the output reaches the bottom of the window.
		  (setq ess-comint-scroll-to-bottom-on-output t)

		  ;; I do not want ess to insert <- when _ is
		  ;; pressed. I use _ far more often than <-, which I
		  ;; only use for function declarations, not for
		  ;; setting other variables. 
		  (ess-toggle-underscore nil)

		  ;; Some custom hooks
		  (add-hook 'ess-mode-hook 
			    (lambda () 
			      (setq truncate-lines t)
			      (auto-fill-mode)))

		  ;; r-autoyas is an interesting mode for
		  ;; auto-creating yasnippet completions. Mostly, it
		  ;; just gets in the way as it only works for some
		  ;; functions. I prefer to use my own custom
		  ;; snippets.

		  ;; (require 'r-autoyas) (add-hook 'ess-mode-hook
		  ;; 'r-autoyas-ess-activate)
		  ))))

;; Taken from Section 4.5 of the ESS manual:
(eval-after-load "comint" 
  '(progn 
     ;; The following makes the up/down keys behave like typical
     ;; console windows: for cycling through previous commands
     (define-key comint-mode-map [up] 
       'comint-previous-matching-input-from-input) 
     (define-key comint-mode-map [down] 
       'comint-next-matching-input-from-input) 

     ;; Make C-left and A-left skip the R prompt at the beginning of
     ;; line
     (define-key comint-mode-map [A-left] 'comint-bol)               
     (define-key comint-mode-map [C-left] 'comint-bol)               

     ;; This ensures that the R process window scrolls automatically
     ;; when new output appears (otherwise you're scrolling manually
     ;; all the time).
     (setq comint-scroll-to-bottom-on-output 'others) 
     (setq comint-scroll-show-maximum-output t)
     ;; Somewhat extreme, almost disabling writing in *R*, *shell*
     ;; buffers above prompt
     (setq comint-scroll-to-bottom-on-input 'this)))

(provide 'setup-ess-mode)

;; setup-ess-mode ends here
