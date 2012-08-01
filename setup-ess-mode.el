;; This loads a more recent build of ESS-mode, much newer than the
;; version packaged with Aquamacs
(load "/Applications/Aquamacs.app/Contents/Resources/lisp/ess-12.04/lisp/ess-site")

(require 'ess-site)

;; The following line was added so that when using iESS to interact
;; with R, the process buffer running R will always scroll 
;; automatically when the output reaches the bottom of the window.
(setq ess-comint-scroll-to-bottom-on-output t)

(add-hook 'ess-mode-hook (lambda () 
			   (setq truncate-lines t)))

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
