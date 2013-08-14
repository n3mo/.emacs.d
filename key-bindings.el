;; Completion that uses many different methods to find options. I'm
;; using this (which is built into emacs anyways) because I'm using
;; the Apple command key as my meta key. Because application switching
;; happens with the sequence CMD-Tab, I lose the traditional
;; completion (e.g., in elisp) when pressing this
;; combination. Instead, use "C-." Repeated pressings will cycle
;; through possible completions as usual
(global-set-key (kbd "C-.") 'hippie-expand)

;; This keybinding uses ido completion for opening recent files
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open). This replaces
;; the built function 'find-file-read-only' set by default to this
;; keybinding. 
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Flyspell has a great function that checks the spelling of the last
;; highlighted word that you typed (rather than the last word that you
;; typed). The command lists possible corrections at the top of the
;; frame and allows you to quickly choose the correct spelling by
;; pressing a number. This is useful enough that I believe it deserves
;; it's own keybinding. I'm using "C-c j" as it is otherwise an open
;; binding, and I saw jonshea recommend it on emacswiki (under
;; CustomizeAquamacs, of all places)
(global-set-key (kbd "C-c j") 'flyspell-check-previous-highlighted-word)

;; This makes use of a custom function defined in .emacs.d/init.el
(global-set-key "\C-ci" 'ido-goto-symbol) ; or any key you see fit

;; Set zap-up-to-char (see init.el) to the binding traditionally used
;; for zap-to-char
(global-set-key "\M-z" 'zap-up-to-char)

;; Use iBuffer in place of the standard list buffers function
;;(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Inset file name at point. See init.el for the defun
(global-set-key "\C-c\C-i" 'my-insert-file-name)

;; Ace jump mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;; If you also use viper mode :
;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)

;; Expand region by semantic units
(global-set-key (kbd "C-=") 'er/expand-region)

;; Multiple cursors keybindings (installed thru
;; list-packages). Suggestions for keybindings found at github, but
;; I use my own tweaks
;; https://github.com/magnars/multiple-cursors.el
;; From active region to multiple cursors:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;; I also bind the same function to the more friendly option that is
;; close to expand-region (and this also doesn't require the shift
;; key). This replaces the built-in keybinding for
;; toggle-input-method, something that I am very unlikely to need
;; often. 
(global-set-key (kbd "C-\\") 'mc/mark-next-like-this)
;; This overwrites the built-in binding to delete-horizontal-space. I
;; never use this, but use instead the similar just-one-space bound to
;; M-space 
(global-set-key (kbd "M-\\") 'mc/mark-all-like-this)
;; Some others I could use
;; (global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-Æ") 'mc/mark-more-like-this-extended)
;; (global-set-key (kbd "M-å") 'mc/mark-all-in-region)

;; The following function bound to this key sequence is defined in
;; .emacs.d/plugins/my-functions.el 
(global-set-key (kbd "C-c r") 'replace-last-sexp)

;; Org-mode global key bindings
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)

;; Linum-mode easy access. Also, an easy switch between relative and
;; absolute line numbering
(global-set-key (kbd "C-c l") 'linum-mode)
(global-set-key (kbd "C-c C-l") 'linum-relative-toggle)

;; Shortcuts for move-text (installed thru melpa)
(global-set-key (kbd "<M-up>") 'move-text-up)
(global-set-key (kbd "<M-down>") 'move-text-down)

;; This clones the current line, commenting out the first instance,
;; and returns point to where you were.
(global-set-key (kbd "C-c C-w") 'copy-and-comment-region)

;; This runs my custom underline-text function for creating easy
;; section headings. Prefix with the universal argument to get
;; prompted for character type.
(global-set-key (kbd "C-c u") 'underline-text)

;; Keybindings to my own convenience insertion functions
(global-set-key (kbd "C-c f d") 'n-insert-degree)
(global-set-key (kbd "C-c f t") 'n-timestamp)

(provide 'key-bindings)
