;; The following lines points to the custom org-mode installation that I 
;; performed to get up to date. I did so because the default
;; org-mode installed is often highly outdated

;; I used to manually install org-mode. Now, I use the Emacs 24+
;; package system. This was my old method of targeting my custom install.
;; (setq load-path (cons "~/main/org/emacs-app/org-mode/lisp" load-path))

;; The following line shouldn't be necessary, but I found it necessary
;; to get the elpa-installed org mode to take precedence over the
;; built-in version. This is a shame, as every time I update org-mode
;; with package.el, I will need to manually change the load-path
;; below. 
;; (add-to-list 'load-path "~/.emacs.d/elpa/org-20141222")
;; (require 'org-install) ;; This also shouldn't be necessary...

;; The local-unset-key in this hook removes the org-mode binding that
;; shadows what I have set for ace-jump-mode ("C-c SPC").
(add-hook 'org-mode-hook 
	  (function (lambda ()
		      (flyspell-mode)
		      (visual-line-mode)
		      ;(auto-fill-mode)
		      (local-unset-key (kbd "C-c SPC")))))

;; The following adds my own custom template expansion(s). Start a
;; line with <r (or replace r with another key) and press TAB to
;; generate the text I've supplied.
(eval-after-load 'org
  '(progn
     ;; This R template exports code and results only
     (add-to-list 'org-structure-template-alist
		  '("r" "#+BEGIN_SRC R :exports both :results output\n?\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))
     ;; This R template generates a code block for exporting figures
     (add-to-list 'org-structure-template-alist
		  '("R" "#+BEGIN_SRC R :exports both :results graphics :file ./fig_1?.png\n\n#+END_SRC" "<src lang=\"?\">\n\n</src>"))))

(setq org-directory "~/main/org")
(setq org-default-notes-file (concat org-directory "/todo.org"))

;; Org capture settings for capturing tasks, journal entries, etc.
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory
						 "/todo.org") "Tasks") 
	 "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree (concat org-directory
						    "/journal.org"))
	 "* %?\nEntered on %U\n  %i\n  %a")
	("k" "Kwotes" item (file+headline
			    (concat org-directory "/quotes.org")
			    "Unsorted Quotes"))))

;;;;;;;;;; ORG-BABEL CONFIGURATION
;; stop C-c C-c within code blocks from querying
(setq org-confirm-babel-evaluate nil)

;; fontify code blocks in org files
(setq org-src-fontify-natively t)

;; Babel language support for org-mode. Babel allows you to embed
;; code directly into org files, but you must first specify which
;; languages to allow. Emacs-lisp should be enabled by default... I
;; specify it anyways... because it makes me happy.
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (sh . t)
   (emacs-lisp . t)
   (python . t)))

(provide 'setup-org-mode)
