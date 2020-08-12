;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:      amsreftex.el
;; Description:   We attempt to extend reftex to handle amsrefs bibliographies.
;;                
;; Author:        Fran Burstall <feb@maths.bath.ac.uk>
;; Created at:    Wed Jan  3 21:29:31 2018
;; Modified at:   Wed Aug 12 16:00:08 2020
;; Modified by:   Fran Burstall <feb@maths.bath.ac.uk>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)
(require 'reftex)

;; An amsrefs entry is modelled  by an a-list, collecting authors and editors into
;; lists of same.
(defun amsreftex-extract-fields (blob &optional prefix)
  "Scan string BLOB for key-value pairs and collect these in an a-list.

Prefix key with string \"PREFIX-\" if PREFIX is non-nil.

Fields with keys 'author' or 'editor' are collected into lists of same."
  (with-temp-buffer
    (fundamental-mode)
    (set-syntax-table reftex-syntax-table-for-bib)
    (insert blob)
    (goto-char (point-min))
    (let (alist start stop key field authors editors)
      (while (re-search-forward "\\(\\(?:\\w\\|-\\)+\\)[ \t\n\r]*=[ \t\n\r]*{"
				nil t)
	(setq key (downcase (reftex-match-string 1)))
	;; (forward-char 1)
	(setq start (point))
	(condition-case nil
	    (up-list 1)
	  (error nil))
	;; extract field value
	(let ((stop (1- (point))))
	  (setq field (buffer-substring-no-properties start stop)))
	;; remove extra whitespace
	(while (string-match "[\n\t\r]\\|[ \t][ \t]+" field)
	  (setq field (replace-match " " nil t field)))
	;; collect fields
	(cond
	 ((equal key "author")
	  (push field authors))
	 ((equal key "editor")
	  (push field editors))
	 ((equal key "book")
	  ;; compound field so recurse
	  (setq alist (append (amsreftex-extract-fields field "book") alist)))
	 (t
	  (push (cons (concat prefix (when prefix "-") key) field) alist))))
      (when authors
	(push (cons (concat prefix (when prefix "-") "authors") (nreverse authors)) alist))
      (when editors
	(push (cons (concat prefix (when prefix "-") "editors") (nreverse editors)) alist))
      (nreverse alist))))


(defun amsreftex-parse-entry (entry &optional from to)
  "Parse amsrefs ENTRY.
If ENTRY is nil then parse the entry in current buffer between FROM and TO."
  (let (alist)
    (save-excursion
      (save-restriction
	(if entry
	    (progn
	      (set-buffer (get-buffer-create " *RefTeX-scratch*"))
	      (fundamental-mode)
	      (set-syntax-table reftex-syntax-table-for-bib)
	      (erase-buffer)
	      (insert entry))
	  (widen)
	  (if (and from to) (narrow-to-region from to)))
	(goto-char (point-min))
	(if (re-search-forward "\\\\bib[*]?{\\(\\(?:\\w\\|\\s_\\)+\\)}{\\(\\w+\\)}{" nil t)
	    (setq alist
		  (list
		   (cons "&type" (downcase (reftex-match-string 2)))
		   (cons "&key" (reftex-match-string 1)))))
	(append alist (amsreftex-extract-fields (buffer-string)))))))

;; reftex-get-bib-field returns the empty string when the field is not
;; present.  This makes testing for presence more verbose.  So we
;; return nil in this case.
(defun amsreftex-get-bib-field (field entry)
  "Return value of field FIELD in ENTRY or nil if FIELD is not present."
  (cdr (assoc field entry)))

(defun amsreftex-extract-authors (entry)
  "Deliver list of ENTRY's authors, or, failing that, editors."
  (if-let ((authors (amsreftex-get-bib-field "authors" entry)))
      authors
    (amsreftex-get-bib-field "editors" entry)))

(defun reftex-format-amsrefs-entry (entry)
  "Format a amsrefs ENTRY so that it is nice to look at."
  (let*
      ((authors (mapconcat (lambda (author) (car (split-string author ",")))
			   (amsreftex-extract-authors entry) ", "))
       (year (if-let ((date (amsreftex-get-bib-field "date" entry)))
		 (car (split-string date "-" t))))
       (title (amsreftex-get-bib-field "title" entry))
       (journal (amsreftex-get-bib-field "journal" entry))
       ;; there are two places to look for the title of a containing book
       (booktitle (if-let ((bt (amsreftex-get-bib-field "booktitle" entry)))
		      bt
		    (amsreftex-get-bib-field "book-title" entry)))
       (eprint (amsreftex-get-bib-field "eprint" entry))
       (volume (amsreftex-get-bib-field "volume" entry))
       (pages (amsreftex-get-bib-field "pages" entry))
       (publisher (amsreftex-get-bib-field "publisher" entry))
       (thesis-type (amsreftex-get-bib-field "type" entry))
       (school (amsreftex-get-bib-field "organization" entry))
       
       (type (amsreftex-get-bib-field "&type" entry))
       (key (amsreftex-get-bib-field "&key" entry))
       (extra
	(cond
	 ((equal type "article")
	  ;; For amsrefs, almost everything is an article, whether in
	  ;; a journal or a conference proceedings or a preprint.
	  ;; This means we must work a little harder to find out where
	  ;; the article appears.  We go with the 'journal' field if
	  ;; present and failing that, we try 'booktitle' and
	  ;; 'eprint'.
	  (cond
	   (journal (concat journal
			    (when volume (format " %s" volume))
			    (when pages (format ", %s" pages))))
	   (booktitle (concat (format "in: %s" booktitle)
			      (when pages (format ", %s" pages))))
	   (eprint (format "eprint: %s" eprint))
	   (t "")))
	 ((equal type "book")
	  (concat "book" (when publisher (format " (%s)" publisher))))
	 ((equal type "thesis")
	  (concat thesis-type (when (and thesis-type school) (format ": %s" school))
		  ": " (amsreftex-get-bib-field "organization" entry)))
	 (t ""))))

    (setq authors (reftex-truncate authors 30 t t))
    (when (reftex-use-fonts)
      (put-text-property 0 (length key) 'face reftex-label-face
			 key)
      (put-text-property 0 (length authors) 'face reftex-bib-author-face
			 authors)
      (put-text-property 0 (length year) 'face reftex-bib-year-face
			 year)
      (put-text-property 0 (length title) 'face reftex-bib-title-face
			 title)
      (put-text-property 0 (length extra) 'face reftex-bib-extra-face
			 extra))
    (concat key "\n     " authors " " year " " extra "\n     " title "\n\n")))

;; samples for testing
(setq amsrefs-entry "\\bib{BraDor09}{article}{
      author={Brander, David},
      author={Dorfmeister, Josef},
       title={Generalized {DPW} method and an application to isometric
  immersions of space forms},
        year={2009},
        ISSN={0025-5874},
     journal={Math. Z.},
      volume={262},
      number={1},
       pages={143\\ndash 172},
         url={http://dx.doi.org/10.1007/s00209-008-0367-9},
      review={\\MR{2491604 (2009m:37186)}},
}
")

(setq amsrefs-entry-1 "\\bib{burstall_isothermic_2006}{article}{
      author={Burstall, F.~E.},
       title={Isothermic surfaces: conformal geometry, {C}lifford algebras and
  integrable systems},
date={2006},
       pages={1\\ndash 82},
       review={\\MR{2222512}},
        doi={10.1090/amsip/036/01},
        book={                  
          title={Integrable systems, geometry, and topology},
      editor={Terng, Chuu-Lian},
      series={AMS/IP Stud. Adv. Math.},
      volume={36},
   publisher={Amer. Math. Soc.},
     address={Providence, RI},},
}
")
e


(setq entry (amsreftex-parse-entry amsrefs-entry-1))

(reftex-format-amsrefs-entry entry)

(cond ((amsreftex-get-bib-field "book-title" entry) "Yes"))

entry


(assoc "authors" entry)
