;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:      reftex-amsrefs.el
;; Description:   We attempt to extend reftex to handle amsrefs bibliographies.
;;                
;; Author:        Fran Burstall <feb@maths.bath.ac.uk>
;; Created at:    Wed Jan  3 21:29:31 2018
;; Modified at:   Thu Jan  4 22:25:42 2018
;; Modified by:   Fran Burstall <feb@maths.bath.ac.uk>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

(defun reftex-parse-amsref-entry (entry &optional from to raw)
  "Parse amsrefs ENTRY.
If ENTRY is nil then parse the entry in current buffer between FROM and TO.
If RAW is non-nil, keep double quotes/curly braces delimiting fields."
  (let (alist key start field)
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
                   (cons "&key"  (reftex-match-string 1)))))
        (while (re-search-forward "\\(\\(?:\\w\\|-\\)+\\)[ \t\n\r]*=[ \t\n\r]*"
				  nil t)
          (setq key (downcase (reftex-match-string 1)))
          (cond
           (raw
            (setq start (point))
            (forward-char 1))
           (t
            (forward-char 1)
            (setq start (point))
            ))
	  (condition-case nil
                (up-list 1)
              (error nil))
          
          ;; extract field value, ignore trailing comma if in RAW mode
          (let ((stop (if (and raw (not (= (char-after (1- (point))) ?,)))
                          (point)
                        (1- (point))) ))
            (setq field (buffer-substring-no-properties start stop)))
          ;; remove extra whitespace
          (while (string-match "[\n\t\r]\\|[ \t][ \t]+" field)
            (setq field (replace-match " " nil t field)))
          (push (cons key field) alist))))
    (nreverse  alist)))
;; TO DO: do amsref and bibtex in one shot

(defun reftex-get-amsrefs-names (field entry)
  "Return a list with the author or editor names in ENTRY.
If FIELD is empty try \"editor\" field."
  (let* ((field-count (cl-count-if (lambda (x) (equal (car x) field)) entry))
	 (name-type (if (> field-count 0) field "editor"))
	 (name-count (if (equal name-type field)
			 field-count
		       (cl-count-if (lambda (x) (equal (car x) name-type)) entry)))
	 (spaces split-string-default-separators))
    (cond
     ((> name-count 1)
      (cl-loop for item in entry
	       if (equal (car item) name-type)
	       collect (car (split-string (cdr item) "," t spaces))))
     ((equal name-count 1)
      (mapcar (lambda (x) (car (split-string x "," t)))
	      (split-string (reftex-get-bib-field name-type entry) "\\band\\b" t
			    spaces)))
     (t ""))))


;; NEXT: use this to replace reftex-get-bib-names


(defun reftex-format-amsrefs-entry (entry)
  "Format a amsrefs ENTRY so that it is nice to look at."
  (let*
      ((auth-list (reftex-get-amsrefs-names "author" entry))
       (authors (mapconcat 'identity auth-list ", "))
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

