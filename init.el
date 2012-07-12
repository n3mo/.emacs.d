;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; The following line adds my customization .el scripts to the
;; search path.
(add-to-list 'load-path "~/.emacs.d/plugins")

;; Load my custom functions 
(load "my-functions.el")

;; Don't load the startup screen. It's obnoxious
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)

;; Emacs for mac os x doesn't find my bash PATH, so I've specified it
;; here manually
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
;; problems can also be fixed by launchin Emacs.app from the terminal,
;; rather than from the doc or finder. Launching emacs.app in that
;; way ensures my full paths are loaded properly)
(setq exec-path (append exec-path '("/sw/bin")))

;; Mac OS  specific changes
;; change command to meta, and ignore option to use weird Norwegian keyboard
(setq mac-option-modifier 'none)
(setq mac-command-modifier 'meta)

;; CUA mode is great. Adds many features I can't live without at this point
(cua-mode t)
(cua-selection-mode t)

;; Set path to .emacs.d
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
;; Set up load path
(add-to-list 'load-path dotfiles-dir)

;; Setup extensions
(load "setup-org-mode")
(load "setup-ess-mode")
(load "key-bindings")

;; Enable recent files mode to open recently opened files
(require 'recentf)
(recentf-mode 1)
;; get rid of `find-file-read-only' and replace it with something
;; more useful.
 
;; 25 files ought to be enough.
(setq recentf-max-saved-items 25)
 
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

; I hate the audible alarm bell that emacs signals all the time. I
;; swtich to a visual bell here
(setq visible-bell 1)
;; To shut off the alarm bell completely, uncomment the following line
;; instead
;; (setq ring-bell-function 'ignore)

;; Add color to a shell running in emacs 'M-x shell'
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; This loads tab completion functionality much like Textmate
;; by sourcing the relevant file
(add-to-list 'load-path "~/.emacs.d/plugins")
(require 'yasnippet-bundle)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;; Get rid of the tool bar that is default on (i.e., this toggles it
;; off). THIS IS DONE ABOVE MORE ELEGENTLY 
;; (tool-bar-mode -1)

;; Aquamacs provides its own mechanisms for adjusting color themes. These
;; interfere with the method I prefer. To fix this, I manually set the color
;; theme here
(require 'color-theme)
(color-theme-initialize)
(setq color-theme-is-global t)
(color-theme-calm-forest)

;; Turn off the scroll bar in emacs. THIS IS DONE MORE ELEGENTLY ABOVE
;; (set-scroll-bar-mode 'nil)
;; (set-scroll-bar-mode 'right)
;; (set-scroll-bar-mode 'left)
(put 'upcase-region 'disabled nil)

;; ace-jump-mode. This minor mode provides quick navigation through
;; the buffer by jumping to target words, characters, or lines. The
;; mode is mainted on github at
;; https://github.com/winterTTr/ace-jump-mode 
(add-to-list 'load-path "~/.emacs.d/plugins/")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
;;
;; ;;If you also use viper mode :
;; (define-key viper-vi-global-user-map (kbd "SPC") 'ace-jump-mode)

;; This code adds ido tab completion to imenu, making it quick and
;; easy to jump to functions and other objects indexed by imenu.
(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END of IDO imenu support

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

;; Matlab supprt for emacs
(add-to-list 'load-path "~/.emacs.d/plugins/matlab-emacs")
(load-library "matlab-load")
(add-to-list
 'auto-mode-alist
 '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)

;; The following line is necessary to get the "matlab-shell" command
;; to start without loading the desktop. Uncomment the next line to
;; prevent java from loading. This will produce errors during plotting. 
;; In the future this cause errors. Use the second line to allow java
;; to load, but to prevent the desktop GUI from loading. This is slower,
;; but is the currently supported method for doing things.
;; (setq matlab-shell-command-switches '("-nojvm -nosplash"))
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))

(put 'dired-find-alternate-file 'disabled nil)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-view-program-list (quote (("SystemDefault" "open %o"))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "SystemDefault") (output-html "SystemDefault"))))
 '(cua-mode t nil (cua-base))
 '(mediawiki-site-alist (quote (("Lab" "http://cogmod.osu.edu/plwiki/" "nvanhorn" "p4plwin1211" "Main Page"))))
 '(org-agenda-files (quote ("~/main/org/todo.org")))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; I have manually installed AUCTeX for latex management. It was
;; installed in Emacs.app/Contents/Resources/site-lisp, which is on
;; the load path already. Still, it must be loaded to be usable in
;; emacs: 
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
		     (define-key message-mode-map "\C-c\t" 'external-abook-try-expand)))))

;; This enables automatic (d)encryption of files ending in .gpg using
;; EasyPG, which is included in emacs as of version 23. I'm using this
;; encrypt my gnus passwords
(require 'epa-file) 
;; (epa-file-enable)
(setq epg-gpg-program "/usr/local/bin/gpg")  ;; I manually specify the binary files here

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
(setq browse-url-browser-function 'browse-url-generic
browse-url-generic-program "open")

;; Set startup frame dimensions
;; (add-to-list 'default-frame-alist '(height . 90))
;; (add-to-list 'default-frame-alist '(width . 80))

;; Load haskell-mode files
(load "~/.emacs.d/plugins/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; These indentation modes are mutually exclusive! Only activate
;; one of the following three lines!
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)


;; Expand region
(add-to-list 'load-path "~/.emacs.d/plugins/expand-region.el/")
(require 'expand-region)

;; An extended re-builder that supports perl regexs
(load "~/.emacs.d/plugins/re-builder-X")

;; The insidious big brother database
(add-to-list 'load-path "~/.emacs.d/plugins/bbdb-2.35/lisp")
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
