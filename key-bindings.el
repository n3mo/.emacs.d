;; Completion that uses many different methods to find options. I'm
;; using this (which is built into emacs anyways) because I'm using
;; the Apple command key as my meta key. Because application switching
;; happens with the sequence CMD-Tab, I lose the traditional
;; completion (e.g., in elisp) when pressing this
;; combination. Instead, use "C-." Repeated pressings will cycle
;; through possible completions as usual
(global-set-key (kbd "C-.") 'hippie-expand)

;; This keybinding uses ido completion for opening recent files
;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)
;; The following keybinding sets up recentf to behave like Aquamacs default
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

;; Expand region by semantic units
(global-set-key (kbd "C-=") 'er/expand-region)
