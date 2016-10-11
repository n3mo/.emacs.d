;; I REALLY like geiser-mode for editing scheme. Unfortunately, it
;; only works with Guile and Racket. This is frustrating when I'm
;; editing (for example) Chicken scheme and Geiser-mode keeps
;; activating. Setting this to nil ensures that geiser-mode does not
;; activate automatically
;; (setq geiser-mode-auto-p nil)
;; (setq geiser-active-implementations '(chicken racket guile))
;; (setq geiser-impl-installed-implementations '(chicken racket guile))
;; (setq geiser-chicken-binary "/home/nemo/bin/bin/csi -:c")

;; I've been experimenting with various flavors of scheme. This sets
;; the default binary to run when an inferior scheme lisp repl is
;; called 
;; (setq scheme-program-name "csi -:c")  ;; Chicken scheme
(setq scheme-program-name "racket")  ;; Racketscheme

;; I'm experimenting with various chicken-scheme modes. Currently, I'm
;; using cluck, which is a chicken-specific fork of quack-mode:
;; https://github.com/ddp/cluck
;; (add-to-list 'load-path "~/.emacs.d/plugins/cluck/")
;; (require 'cluck)
;; (require 'quack)

;; Chicken-doc customization for finding documentation of sexp at
;; point
;; (defun chicken-doc (&optional obtain-function)
;;   (interactive)
;;   (let ((func (funcall (or obtain-function 'current-word))))
;;     (when func
;;       (process-send-string (scheme-proc)
;;                            (format "(require-library chicken-doc) ,doc %S\n" func))
;;       (save-selected-window
;;         (select-window (display-buffer (get-buffer scheme-buffer) t))
;;         (goto-char (point-max))))))

;; (eval-after-load 'cmuscheme
;;   '(define-key scheme-mode-map "\C-cd" 'chicken-doc))
;; (eval-after-load 'cmuscheme
;;  '(define-key inferior-scheme-mode-map "\C-cd"
;;     (lambda () (interactive) (chicken-doc 'sexp-at-point))))

;; Or better yet, chicken-slime integration!
;; Where Eggs are installed
(add-to-list 'load-path "/usr/local/lib/chicken/6/")
(autoload 'chicken-slime "chicken-slime" "SWANK backend for Chicken" t)
;; We can now load functionality by running the function chicken-slime
;; in Emacs
;; (add-hook 'scheme-mode-hook
;;           (lambda ()
;;            (slime-mode f)
;; 	   (auto-fill-mode t)))

(add-hook 'scheme-mode-hook
          (lambda ()
	    (auto-fill-mode t)))

(provide 'setup-scheme-mode)
