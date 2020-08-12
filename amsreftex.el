;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:      amsreftex.el
;; Description:   We attempt to extend reftex to handle amsrefs bibliographies.
;;                
;; Author:        Fran Burstall <feb@maths.bath.ac.uk>
;; Created at:    Wed Jan  3 21:29:31 2018
;; Modified at:   Wed Aug 12 13:39:49 2020
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
  (let (alist key start field authors editors)
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
	(amsreftex-extract-fields (buffer-string))))))


(defun reftex-format-amsrefs-entry (entry)
  "Format a amsrefs ENTRY so that it is nice to look at."
  (let*
      ((authors (mapconcat (lambda (author) (car (split-string author ",")))
			   (reftex-get-bib-field "authors" entry) ", "))
       (year      (or (car (split-string (reftex-get-bib-field "date" entry) "-" t))
		      (reftex-get-bib-field "year" entry)))
       (title     (reftex-get-bib-field "title" entry))
       (type      (reftex-get-bib-field "&type" entry))
       (key       (reftex-get-bib-field "&key"  entry))
       (extra
        (cond
         ((equal type "article")
          (concat (let ((jt (reftex-get-bib-field "journal" entry)))
                    ;; biblatex prefers the alternative journaltitle
                    ;; field, so check if that exists in case journal
                    ;; is empty.
                    (if (zerop (length jt))
                        (reftex-get-bib-field "journaltitle" entry)
                      jt))
                  " "
                  (reftex-get-bib-field "volume" entry) ", "
                  (reftex-get-bib-field "pages" entry)))
         ((equal type "book")
          (concat "book (" (reftex-get-bib-field "publisher" entry) ")"))
         ((equal type "phdthesis")
          (concat "PhD: " (reftex-get-bib-field "school" entry)))
         ((equal type "mastersthesis")
          (concat "Master: " (reftex-get-bib-field "school" entry)))
         ((equal type "inbook")
          (concat "Chap: " (reftex-get-bib-field "chapter" entry)
                  ", pp. " (reftex-get-bib-field "pages"   entry)))
         ((or (equal type "conference")
              (equal type "incollection")
              (equal type "inproceedings"))
          (reftex-get-bib-field "booktitle" entry "in: %s"))
         (t ""))))
    (setq authors (reftex-truncate authors 30 t t))
    (when (reftex-use-fonts)
      (put-text-property 0 (length key)     'face reftex-label-face
                         key)
      (put-text-property 0 (length authors) 'face reftex-bib-author-face
                         authors)
      (put-text-property 0 (length year)    'face reftex-bib-year-face
                         year)
      (put-text-property 0 (length title)   'face reftex-bib-title-face
                         title)
      (put-text-property 0 (length extra)   'face reftex-bib-extra-face
                         extra))
    (concat key "\n     " authors " " year " " extra "\n     " title "\n\n")))
;; Next: booktitle if no jnl for article
;; theses
;; conference type things (need examples here)

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


(setq entry (reftex-parse-amsref-entry amsrefs-entry))

(reftex-format-amsrefs-entry entry)

