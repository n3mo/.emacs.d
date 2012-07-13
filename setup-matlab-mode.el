;; Matlab customizations

;; My matlab-mode installation location
(add-to-list 'load-path "~/.emacs.d/plugins/matlab-emacs")

;; Matlab support for emacs
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
