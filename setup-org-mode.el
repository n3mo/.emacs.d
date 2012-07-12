;; The following lines points to the custom org-mode installation that I 
;; performed to get up to date. I did so because the default
;; org-mode installed is often highly outdated
(setq load-path (cons "~/main/org/emacs-app/org-mode/lisp" load-path))
(require 'org-install)

(add-hook 'org-mode-hook 
	  (lambda ()
	    (flyspell-mode)
	    (visual-line-mode)
	    (auto-fill-mode)))

(setq org-directory "~/main/org")
(setq org-default-notes-file (concat org-directory "/todo.org"))
;; These set global bindings, available always in emacs, regardless of mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;;;;;;;;;; ORG-BABEL CONFIGURATION
;; stop C-c C-c within code blocks from querying
(setq org-confirm-babel-evaluate nil)

;; Babel language support for org-mode. Babel allows you to embed
;; code directly into org files, but you must first specify which
;; languages to allow.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (sh . t)
   (emacs-lisp . t)
   ))
