;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Don't load the startup screen. It's obnoxious
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Add the necessary directories to my load path. Some additional
;; dependencies unique to certain modes may also be loaded in their
;; respective files (e.g., I put the load path for the matlab-mode
;; installation in the startup file "setup-matlab-mode.el")
(add-to-list 'load-path "~/.emacs.d/plugins")
(add-to-list 'load-path "~/.emacs.d/plugins/bibtex-to-plain-text.el")
(add-to-list 'load-path "~/.emacs.d/plugins/bbdb-2.35/lisp")
(add-to-list 'load-path "~/.emacs.d/plugins/r-autoyas")
(add-to-list 'load-path "/Applications/Emacs.app/Contents/Resources/site-lisp")
(add-to-list 'load-path "~/.emacs.d/plugins/noob-arrows-mode.el/")

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" dotfiles-dir))
(load custom-file)

;; Load my custom elisp functions file
(load "my-functions")

;; Most of my initialization script has been split into separate files
;; in .emacs.d/ . Here they are...
(require 'setup-org-mode)
(require 'setup-ess-mode)
(require 'setup-ido-mode)
(require 'setup-matlab-mode)
(require 'mac)
(require 'bibtex-to-plain-text)
(require 'key-bindings)
(require 'org-tree-slide)
(require 'noob-arrows-mode)

;; CUA mode is great. Adds many features I can't live without at this point
;; (cua-mode t)
(cua-selection-mode t) ; I don't want to C-x C-v cua functionality

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Enable recent files mode to open recently opened files
(require 'recentf)
(recentf-mode 1)
;; 25 files ought to be enough.
(setq recentf-max-saved-items 25)

; I hate the audible alarm bell that emacs signals all the time. I
;; swtich to a visual bell here
(setq visible-bell 1)
;; To shut off the alarm bell completely, uncomment the following line
;; instead
;; (setq ring-bell-function 'ignore)

(add-hook 'emacs-lisp-mode-hook 'auto-fill-mode)

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; This loads tab completion functionality much like Textmate
;; by sourcing the relevant file. Yay yasnippet!
(require 'yasnippet-bundle)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;; I should probably get rid of the following eventually and switch to
;; the new theme support built into Emacs 24+
;; (require 'color-theme)
;; (color-theme-initialize)
;; (setq color-theme-is-global t)
;; (color-theme-calm-forest)

;; As above indicates, I'm making the move away from the
;; color-theme-package (not because I really hated it, but because
;; Emacs has built in support now that I'd rather use). My custom
;; added themes are in the following directory
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/cyberpunk-theme.el/")
;; (load-theme 'wombat t) ;; Previous favorite
(load-theme 'cyberpunk t)  ;; Taken from Emacs-live

;; Enable disabled commands
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Ace-jump-mode. This minor mode provides quick navigation through
;; the buffer by jumping to target words, characters, or lines. The
;; mode is mainted on github at
;; https://github.com/winterTTr/ace-jump-mode. The keybinding is
;; defined in "key-bindings.el"  
(require 'ace-jump-mode)

;; The standard zap-to-char function kills up through and including
;; the provided letter. I'd prefer to have it NOT kill the provided
;; letter, but instead kill everything up to it, leaving point just
;; before the provided letter. This zap-up-to-char function provides
;; this functionality. I also bind it to the default key combo for
;; zap-to-char (in /key-bindings.el)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.
  
  \(fn arg char)"
  'interactive)

;; Historically, the command C-x C-b calls the command (list-buffers) which 
;; opens a buffer called "*Buffer List*". iBuffer is a much improved
;; replacement. I reassign the default behavior of the command to use
;; iBuffer in key-bindings.el
(require 'ibuffer)

;; This bit of code creates an easy way to insert a filename (and its
;; path) into the buffer at point. 
(defun my-insert-file-name (filename &optional args)
  "Insert name of file FILENAME into buffer after point.
  
  Prefixed with \\[universal-argument], expand the file name to
  its fully canocalized path.  See `expand-file-name'.
  
  Prefixed with \\[negative-argument], use relative path to file
  name from current directory, `default-directory'.  See
  `file-relative-name'.
  
  The default with no prefix is to insert the file name exactly as
  it appears in the minibuffer prompt."
  ;; Based on insert-file in Emacs -- ashawley 20080926
  (interactive "*fInsert file name: \nP")
  (cond ((eq '- args)
	 (insert (file-relative-name filename)))
	((not (null args))
	 (insert (expand-file-name filename)))
	(t
	 (insert filename))))

;; I have manually installed AUCTeX for latex management. It was
;; installed in /Applications/Emacs.app/Contents/Resources/site-lisp/,
;; which was added to the load path above. Still, it must be loaded to
;; be usable in emacs:

(load "auctex.el" nil t t) 
(load "preview-latex.el" nil t t)

;; Some default behaviors and minor modes for use with AUCTeX mode
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; htmlize for pretty conversion to html with syntax highlighting. See
;; also htmlfontify. I installed htmlize manually in
;; .emacs.d/plugins. This was necessary for org-mode export to html
;; for syntax highlighting of source code blocks
(require 'htmlize)
(put 'narrow-to-region 'disabled nil)

;; Start an emacs daemon. This allows other programs (or me, manually)
;; to open a file with an already running instantiation of
;; Emacs.app. Note that this is typically accomplished by running the
;; command "emacs --daemon" to start the emacs server, followed by
;; subsequent "emacsclient <file>" commands to pass files to a running
;; emacs process. For Emacs.app, the commands are
;; "/Applications/Emacs.app/Contents/MacOS/Emacs --daemon" and
;; "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient <file>"
;; respectively. Rather than start the server from the terminal, you
;; can also do so with a running emacs process with the command "M-x
;; server-start". Alternatively, you can run that command at startup,
;; which is what I've chosen to do below. Note that another option is
;; to have this happen at boot time on OS X. See this link for
;; details:
;; http://superuser.com/questions/50095/how-can-i-run-mac-osx-graphical-emacs-in-daemon-mode 
(require 'server)
(unless (server-running-p)
  (server-start))

;; This enables automatic (d)encryption of files ending in .gpg using
;; EasyPG, which is included in emacs as of version 23. I'm using this
;; encrypt my gnus passwords
(require 'epa-file) 
;; (epa-file-enable)
;; On my Mac, the gpg is installed in an unusual place. I must specify
;; its location to get things working
(if (eq system-type 'darwin)
    ;; I manually specify the binary files here
    (setq epg-gpg-program "/usr/local/bin/gpg"))

;; Mediawiki mode for wiki pages
(require 'mediawiki)

;; I want emacs to open URLs in Conkeror. As of <2012-05-31 Thu>, URL
;; remoting, as it's called in conkeror, doesn't work. That is, you
;; cannot specify a URL from the command line to conkeror on OS X. The
;; workaround was to set conkeror as my default browser and then use
;; the open command "open -a Conkeror http://www.example.com". Oddly,
;; I could not use CMD-i on an html file in Finder to set conkeror to
;; open all similar files. I had to open Safari, go to preferences,
;; and set Conkeror as the default web browser there.
(if (eq system-type 'darwin)
    (setq browse-url-browser-function 'browse-url-generic
	  browse-url-generic-program "open"))

;; Set startup frame dimensions
;; (add-to-list 'default-frame-alist '(height . 90))
;; (add-to-list 'default-frame-alist '(width . 80))

;; Load haskell-mode files
(load "~/.emacs.d/plugins/haskell-mode/haskell-site-file" t)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; These indentation modes are mutually exclusive! Only activate
;; one of the following three lines!
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Expand region
;; (require 'expand-region)

;; An extended re-builder that supports perl regexs
(load "~/.emacs.d/plugins/re-builder-X")

;; The insidious big brother database
(setq bbdb-file "~/.emacs.d/bbdb")           ;; keep ~/ clean; set before loading
(require 'bbdb) 
(bbdb-initialize)
;; (bbdb-initialize 'gnus 'message)
;; (bbdb-insinuate-message)
(setq 
 bbdb-offer-save 1                        ;; 1 means save-without-asking

 
 bbdb-use-pop-up t                        ;; allow popups for addresses
 bbdb-electric-p t                        ;; be disposable with SPC
 bbdb-popup-target-lines  1               ;; very small
 
 bbdb-dwim-net-address-allow-redundancy t ;; always use full name
 bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

 bbdb-always-add-address t                ;; add new addresses to existing...
 ;; ...contacts automatically
 bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

 bbdb-completion-type nil                 ;; complete on anything

 bbdb-complete-name-allow-cycling t       ;; cycle through matches
 ;; this only works partially

 bbbd-message-caching-enabled t           ;; be fast
 bbdb-use-alternate-names t               ;; use AKA


 bbdb-elided-display t                    ;; single-line addresses

 ;; auto-create addresses from mail
 bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook   
 bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
 ;; NOTE: there can be only one entry per header (such as To, From)
 ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

 '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))

;; Add bbdb GNUS hook
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; Keep all backup ~ files created by emacs in one place
(setq backup-directory-alist '(("." . "~/.saves")))

;; Since my backup ~ files are accumulating in one place and I'm
;; unlikely to check in on them and delete old files, this bit of code
;; will check the age of backup files and remove those that haven't
;; been accessed in over a week
(message "Deleting old backup files...")
(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message "%s" file)
      (delete-file file))))

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; Here are the Emacs lisp repositories that I draw from (for
;; packages-list-packages, etc)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))

;; This is necessary for bibtex-to-plain-text to work
(load "bibtex")

;; This provides reftex with the path to my bibtex files
(setq reftex-bibpath-environment-variables
      '("~/main/work/docs/papers/bib/"))

;; Turn on spell checking when sending messages
(add-hook 'message-mode-hook 'flyspell-mode)

;; The following lines of code are used to add a synonym method for 
;; thesaurus functionality.
;; To use library Synonyms, you will need the Moby Thesaurus II file,
;; `mthesaur.txt', available here:
;;
;; ftp://ibiblio.org/pub/docs/books/gutenberg/etext02/mthes10.zip
(setq synonyms-file "/Users/nemo/.emacs.d/plugins/mthes10/mthesaur.txt")
(setq synonyms-cache-file "/Users/nemo/.emacs.d/plugins/synonym_cache")
(require 'synonyms)

;; On my system (OS X), the program "ls" does not support the --dired
;; flag. By default, dired tries to issue the command "ls --dired"
;; unless the following varible is set to nil. See 
;; C-h v dired-use-ls-dired for more info
(setq dired-use-ls-dired nil)

;; I want to make use of registers more often. Besides those created
;; on the fly, there are a few that I will use often, so they are set
;; here. To jump to a register, use C-x r j e, where the "e" is
;; replaced with the register of choice (the registers are written as
;; ?e, etc. below)
(set-register ?e '(file . "~/.emacs.d/"))
;; My most current project directory
(set-register ?p '(file . "~/main/work/MLExper/WMPL3"))

;; Markdown mode is enabled for file type .md, as this is what's used
;; by github
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Relative line numbers (installed through elpa)
;; (require 'linum-relative)
(autoload 'linum-relative-toggle "linum-relative")

;; Lisp editing functionality (Clojure, racket, guile, etc.)
;; Avoid error buffer while working in nrepl
(setq nrepl-popup-stacktraces nil)

;; Add hooks for paredit mode for various lisp environments
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)

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

;; This sets the default common lisp program
(setq inferior-lisp-program "sbcl")

;; Noob arrows-- my own custom helper function for Emacs newbies
;; This sets the path to the help file to be displayed
;; (setq noob-arrows-help-file
;;       "~/.emacs.d/plugins/noob-arrows-mode.el/noob-arrows-help-file.txt")




;; init.el ends here.
