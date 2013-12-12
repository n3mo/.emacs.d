;; This file includes some custom elisp functions that I've written.
;; You can load this file completely, or load individual functions
;; with eval-region
;;
;; Many of the functions also have aliases defined for ease of use.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       Assorted Utility Functions   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This function takes a region of code and produces html with
;; embedded css suitable for pasting into my markdown-->clojure-->html
;; site generator for creating posts at nicholasvanhorn.com. Because
;; htmlize is used, the current theme determines the css coloring. 
(defun my-htmlize-region (beg end)
  "Htmlize region and put into <pre> tag style that is left in <body> tag
plus add font-size: 10pt"
  (interactive "r")
  (let* ((buffer-faces (htmlize-faces-in-buffer))
         (face-map (htmlize-make-face-map (adjoin 'default buffer-faces)))
         (pre-tag (format
                   "<pre style=\"%s font-size: 10pt\">"
                   (mapconcat #'identity (htmlize-css-specs
                                          (gethash 'default face-map)) " ")))
         (htmlized-reg (htmlize-region-for-paste beg end)))
    (switch-to-buffer-other-window "*htmlized output*")
    ; clear buffer
    (kill-region (point-min) (point-max))
    ; set mode to have syntax highlighting
    (nxml-mode)
    (save-excursion
      (insert htmlized-reg))
    (while (re-search-forward "<pre>" nil t)
      (replace-match pre-tag nil nil))
    (goto-char (point-min))))

;; This function evaluates the sexp before point (similar to
;; eval-last-sexp) and replaces the sexp with its result.
(defun replace-last-sexp ()
  "Evaluate sexp before point; replace the sexp with the
output. Similar to running `eval-last-sexp' with the prefix argument,
  with the addition that the sexp is replaced by the result."
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value))))

(defun google ()
  "Google the selected region if any, display a query prompt otherwise."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
			   (buffer-substring (region-beginning) (region-end))
			 (read-string "Google: "))))))

(defun copy-current-file-name ()
  "Adds the current buffer file name to the kill ring"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "File name '%s' added to the kill ring." filename))))

;; Insert date (formatted as I prefer it)
(defun n-timestamp ()
  "Insert time stamp as I like it into source code at point"
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; Insert degree symbol. I need this often enough that it irritates me
;; to type it manually. This will insert the unicode ° symbol in most
;; buffers, but when latex-mode is active, it will insert ^{\circ} as
;; LaTeX expects it.
(defun n-insert-degree ()
  "Inserts the degree symbol in unicode, or as latex code if the
current major mode is latex-mode"
  (interactive)
  (let ((current-mode (with-current-buffer (current-buffer) major-mode)))
    (if (string= "latex-mode" current-mode)
	(insert "^{\\circ}")
      (insert "°"))))

;; This was taken from a comment on the page
;; https://gist.github.com/prakashk/5319782 where commenter
;; noahfriedman improved upon the gist by accounting for active
;; regions, preserving point, and protecting the kill ring
(defun copy-and-comment-region (beg end)
  "Insert a copy of the lines in region and comment them.
When transient-mark-mode is enabled, if no region is active then only the
current line is acted upon.

If the region begins or ends in the middle of a line, that entire line is
copied, even if the region is narrowed to the middle of a line.
The copied lines are commented according to mode.

Current position is preserved."
  (interactive "r")
  (let ((orig-pos (point-marker)))
  (save-restriction
    (widen)
    (when (and transient-mark-mode (not (use-region-p)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))

    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (unless (= (point) (line-beginning-position))
      (setq end (line-beginning-position 2)))

    (goto-char beg)
    (insert-before-markers (buffer-substring-no-properties beg end))
    (comment-region beg end)
    (goto-char orig-pos))))

;; My own function for quickly underlining section headings. I set
;; this to a key binding in .../.emacs.d/key-bindings.el
(defun underline-text (prefixArgCode)
  "Underlines the current line and moves point to the beginning
of the line directly following the underlining. If
`universal-argument' is called, prompts user for underline
character, otherwise uses the = character."
  (interactive "P")
  (let ((selection (buffer-substring-no-properties
		    (line-beginning-position)
		    (line-end-position)))
	(under-char
	 (if (equal prefixArgCode nil)
	     "="
	   (read-key-sequence "Char?"))))
    (end-of-line)
    (newline-and-indent)
    (insert (apply 'concat (make-list (length selection) under-char)))
    (next-line 1)
    (beginning-of-line)))

;; Taken from Magnar's blog "What the .emacs.d!?". These two functions
;; make it simple to move a line up or down by one. They are bound to
;; C-S-Up and C-S-Down in my keybindings file
;; 
;; I have replaced this functionality with the emacs package
;; move-text, which works on lines and regions. Installed from melpa. 
;; (defun move-line-down ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines 1))
;;     (forward-line)
;;     (move-to-column col)))

;; (defun move-line-up ()
;;   (interactive)
;;   (let ((col (current-column)))
;;     (save-excursion
;;       (forward-line)
;;       (transpose-lines -1))
;;     (move-to-column col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       BibTeX Functions             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following functions interact with BibTeX entries (bibtex mode
;;       does NOT need to be active)

;; This function for easy creation of wiki pages for article entries
;; on the cogmod lab wiki
(defun wiki-update-entry-from-bibtex ()
  "Updates the wiki yasnippet template using information in the bibtex entry at point"
  (interactive)
  (save-excursion
    (setq case-fold-search nil)
    (bibtex-beginning-of-entry)    
    (setq mytitle (mapconcat 'identity (split-string
					(bibtex-text-in-field
					 "title")) " ")) 
    (setq myentry (bibtex-parse-entry))
    (setq mykey (cdr (assoc "=key=" myentry)))
    (beginning-of-buffer)
    (while (search-forward "TITLEHERE" nil t)
      (replace-match mytitle t t))
    (beginning-of-buffer)
    (while (search-forward "NameN11" nil t)
      (replace-match mykey t t))))

;; This function opens a browser tab (or buffer in conkeror) for the
;; doi of the current BibTeX entry. Obviously, point must be inside of
;; a BibTeX entry (and it must contain a field called "doi") for this
;; to work. This is meant to act like the function bibtex-url, which
;; is included by default in bibtex.el. I like to work with doi's
;; directly sometimes, so this extends that functionality to entries
;; that have a doi field but no url field
(defun bibtex-doi ()
  "Browse to url of the current BibTeX entries DOI field"
  (interactive)
  (setq mydoi (bibtex-text-in-field "Doi"))
  (browse-url (concatenate 'string "http://dx.doi.org/" mydoi))
)

;; This function behaves just as bibtex-url (and the function
;; bibtex-doi that I created above). If the current BibTeX entry under
;; point contains a field names "pdf", it will open the file specified
;; in that path string with the default pdf viewer (via the "open"
;; command). Any other suitable viewer could be substituted in the
;; shell-command call below.
(defun bibtex-pdf ()
  "Opens the local pdf file specified in the pdf field of the current
BibTeX entry. The default pdf viewer on Mac OS X is used"
  (interactive)
  (if (null (bibtex-text-in-field "pdf"))
      (message "Missing pdf file path field!")
    ;; Else
    (shell-command (format "open %s" (bibtex-text-in-field "pdf")))
    )
  )

;; This function opertates on the current BibTeX entry under point. If
;; the entry has a field called "pdf", this function will reveal that
;; file's directory in OS X's finder.
(defun bibtex-pdf-reveal-in-finder ()
  "Reveal in OS X Finder the pdf file described in the pdf field of
  the current BibTeX entry"
  (interactive)
  (if (null (bibtex-text-in-field "pdf"))
      (message "Missing pdf file path field!")
    ;; Else
    (setq mylist (split-string (bibtex-text-in-field "pdf") "/"))
    (setq tmp (reverse mylist))
    ;; Remove the file name
    (pop tmp)
    (setq mylist (reverse tmp))
    (shell-command (format "open %s" (mapconcat 'identity mylist
						"/"))) 
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       Org-BibTeX Functions         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following functions add customizations to org-bibtex
;;       interaction. 

;; A function that adds the current org-mode heading's bibtex title to
;; the kill ring.  You can only run this function when point is
;; located somewhere within an org-mode heading that contains a bibtex
;; entry. The entry is actually written using the :PROPERTIES: tag
;; under the org heading. You can call this function from anywhere in
;; the heading, not just within the :PROPERTIES: section
(defun org-bibtex-get-title ()
  "Adds the title of a bibtex entry embedded in an org-mode buffer to the kill ring."
  (interactive)
  (kill-new (org-bibtex-get "title"))
  (message "Title added to kill ring")
)
;; and a shorthand alias version
(defalias 'bt 'org-bibtex-get-title)

;; A function that adds the current org-mode heading's bibtex key to
;; the kill ring. You can only run this function when point is located
;; somewhere within an org-mode heading that contains a bibtex
;; entry. Tthe entry is actually written using the :PROPERTIES: tag
;; under the org heading. You can call this function from anywhere in
;; the heading, not just with the :PROPERTIES: section
(defun org-bibtex-get-key ()
  "Adds the cite key of a bibtex entry embedded in an org-mode buffer to the kill ring."
  (interactive)
  (kill-new (org-bibtex-get "custom_id"))
  (message "Cite key added to kill ring")
)
;; and a shorthand alias version
(defalias 'bk 'org-bibtex-get-key)

;; This assigns the function org-bibtex-read and org-bibtex-write to
;; key bindings. I added this when working on my candidacy reading
;; lists. This is very useful for making org mode notes for each
;; bibtex entry of interest. I chose the key bindings thinking that I
;; could [g]rab the bibtext info command, and then
;; [y]ank it into the org buffer. My method: In an open .tex file,
;; move point to a bibtex key (inside of a /cite{} command) and press
;; "C-c &". This will navigate to the entry in the bibtex file. Once
;; there, use the newly defined keybinding "C-c g" to have org mode
;; grab the bibtex info. Then, jump to the location of interest in
;; your org mode buffer and press the newly defined key binding "C-c
;; y". Org mode will then insert a new heading populated with the info
;; from the bibtex entry! Very handy!
(global-set-key (kbd "C-c g") 'org-bibtex-read)
(global-set-key (kbd "C-c y") 'org-bibtex-write)
