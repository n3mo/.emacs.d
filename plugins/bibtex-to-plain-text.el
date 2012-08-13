;; The variable bibtex-to-plain-text-style controls the behavior of
;; the interactive function `bibtex-create-plain-text-reference'. The
;; way this works is: Each entry type will be represented by an
;; association list. The keyin the alist is the entry type, and the
;; value is a pair of lists. The first item in the pair will consist
;; of a list of the BibTex fields required to format the entry
;; type. The second item will be a formatting string showing how to
;; build the reference in plain text. For example, for APA style
;; formatting of the BibTex entry type 'article', the relevant list
;; would be:
;;
;; ("article" . (("author" "year" "title" "journal" "volume"
;;	           "number" "pages") 
;;	          "%s. (%s). %s. %s, %s%s, %s."))
;;
;; Any conceivable Bibtex type can have a similar list, and these
;; should all be contained in the same association list. The key in
;; each pair will be the BibTex type, and the value will be a list
;; like the one above. Once this is set up the function
;; bibtex-create-plain-text-reference will do the following: 

;; 1 : Determine the type of the Bibtex entry under point 
;; 2 : Consult the association list for the appropriate formatting 
;; 3 : Generate and return the formatted string (and optionally push
;;     the string to the kill ring)

;; Because each entry type in the association list is specified
;; through a formatting string, any notion of style (APA, IEEE, etc.)
;; is hidden from the function. This setup requires some initial work
;; because care must be taken when defining each formatting
;; string. However, this buys the user extra flexibility because even
;; unorthodox styles can be created for specific usages. To support
;; multiple styles, the value of bibtex-to-plain-text-style should be
;; set as an alias to a properly formatted association list containing
;; the formatting lists described above. In principle, the list can
;; have any name. A representative list of APA formatting follows and
;; is used by default. To change this, create your own similar list
;; under a different name, and use 
;;    (setq bibtex-to-plain-text-style your-list-here) 
;; to change the style. 

(setq bibtex-to-plain-text-apa-formats
      '(
	("article" . (("author" "year" "title" "journal" "volume"
			"number" "pages") 
		       "%s (%s). %s. %s, %s|(%s)|, %s."))
	("book" . (("author" "year" "title" "editor" "address"
		     "publisher") 
		    "%s (%s). %s.| %s| |%s: |%s."))))

;; This is what the function bibtex-create-plain-text-reference
;; actually uses. The user can point this to any properly structured
;; association list to change the formatting style, for example from
;; APA to MLA style.
(setq bibtex-to-plain-text-style bibtex-to-plain-text-apa-formats)




;; This function will push a plain text formatted reference to the
;; kill ring based on the current BibTeX entry under point. The
;; purpose of this is for sharing citations with other programs that
;; don't allow me to use LaTeX/BibTeX. Of course, the plain text will
;; need to be formatted manually in the destination application (with
;; italics, etc.). The style and entry types allowable are defined in
;; the association list 
(defun bibtex-create-plain-text-reference (prefixArgCode)
  "Pushes a plain text formatted reference of the current BibTeX
entry to the kill ring. If `universal-argument' is called, the
plain text reference is returned without pushing to the kill
ring. Customize the variable bibtex-to-plain-text-style
to control allowable BibTeX entry types and their corresponding
entry types."
  (interactive "P")
  (save-excursion
    (bibtex-beginning-of-entry)
    (setq myentry (bibtex-parse-entry))
    (setq entry-type (downcase (cdr (assoc "=type=" myentry))))
    (if (member entry-type (mapcar 'car bibtex-to-plain-text-style))
      (progn
	(setq formatted-bibtex-text (format-bibtex-as-plain-text entry-type))
	(when (equal prefixArgCode nil)
	  (kill-new formatted-bibtex-text)
	  (message "Reference pushed to the kill ring"))
	formatted-bibtex-text)
      (message "This BibTeX entry type is not supported!"))))
    

;; This is a helper function that does all the heavy lifting for
;; bibtex-create-plain-text-reference. It takes as input the BibTex
;; entry type and returns the appropriately formatted reference
;; string.
(defun format-bibtex-as-plain-text (entry-type)	   
  "Formats the current BibTeX entry under point according to the
entry-type. Formatting is controlled by the customizable variable
bibtex-to-plain-text-style"
  ;; Find the association values that correspond to the key entry-type
  (setq myEntrySpec (assoc entry-type
			   bibtex-to-plain-text-style)) 
  ;; Isolate the fields that we will be using
  (setq doFields (car (cdr myEntrySpec)))
  ;; Obtain the formatting string
  (setq myFormat (cdr (cdr myEntrySpec)))
  
  ;; This list will store the data from each field as we extract it
  ;; from the entry under point. The contents of this list will be in
  ;; reverse order of how we need them
  (setq entryFields nil)

  ;; Now we work across the fields that we need, grabbing each field's
  ;; data along the way
  (dolist (x doFields) 
    (progn
      ;; Grab the data from the current field, whatever it is
      (setq currData (bibtex-text-in-field x))
      ;; We may need to perform some pre-processing on the text
      (cond
       ((equal "pages" x)
	(progn
	  (setq currData (when (string-match "--" currData)
			   (replace-match "-" nil nil currData)))))
       ((equal "number" x)
	(progn
	  (if (not (null currData))
	      (setq currData (format "%s" currData)))))
       ((equal "editor" x)
	(progn
	  (if (not (null currData))
	      (setq currData (format "(%s, Eds.)." currData))))))
      ;; Clear out newline characters if present. If the entry is
      ;; missing, then this value is currently nil. In this case,
      ;; convert the nil to the empty string "". This ensures that if
      ;; a particular field is missing (e.g., the "editor" of a book),
      ;; then nothing will print in its place, rather than the word
      ;; "nil". This behavior is preferred, as typically when such a
      ;; field is missing, you just omit any mention of it in the
      ;; reference (there could conceivably be exceptions to this,
      ;; which I should check for)
      (if (not (null currData))
	  (setq currData (mapconcat 'identity 
				    (split-string currData) " "))
	;; Else
	(setq currData ""))
      ;; Push the captured data onto our list
      (push currData entryFields)))
  ;; Format the resulting list. This involves three steps in this order: 
  ;; (1) Format the citation according to the rules specified in
  ;;     myFormat.
  ;; (2) Remove empty formatting surrounded by "|" characters. This
  ;;     step removes broken citation formatting resulting from
  ;;     missing fields in the BibTeX entry.
  ;; (3) Remove remaining "|" characters and residual BibTex
  ;;     formatting characters, such as "{" and "}". The remaining "|"
  ;;     result from BibTex entries that DO exist in formatting
  ;;     locations wherein they might not have existed.

  ;; Step 1
  (setq raw-citation (apply 'format (mapconcat '(lambda(x) (format "%s" x)) myFormat "")
	 (reverse entryFields)))
  ;; Step 2
  (setq raw-citation (replace-regexp-in-string
		      "|[[:punct:]]*[^[:alnum:]][[:punct:]]*|" ""
		      raw-citation))
  ;; Step 3
  (replace-regexp-in-string "[|{}]" "" raw-citation))


;; This is a wrapper function that applies the function
;; bibtex-create-plain-text-reference to the entire buffer. The
;; buffer can contain a mixture of BibTeX entries and other unrelated
;; text. Thus, this function will scrape BibTeX entries embedded in
;; text that isn't entirely BibTeX formatted. The results are
;; formatted to APA format (in plain text, however) and are dumped
;; into the special buffer *references*
(defun bibtex-convert-buffer-to-plain-text ()
  "Convert all bibtex entries in the current buffer to plain text
references. This function finds each BibTeX entry and formats it
using `bibtex-create-plain-text-reference'. Results are pushed
into a new buffer called *references*"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (with-output-to-temp-buffer "*references*"
      (while (not (eobp))
	(bibtex-skip-to-valid-entry)
	(setq current-formatted-text
	      (bibtex-create-plain-text-reference t))
	(if (not (eobp)) (next-line))
	(princ (concat current-formatted-text "\n\n")))))
  (message "References written to buffer *references*"))

(provide 'bibtex-to-plain-text)
