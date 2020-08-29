;;; amsreftex.el --- Minor mode to allow reftex to use amsrefs bibliographies  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fran Burstall

;; Author: Fran Burstall <fran.burstall@gmail.com>
;; Keywords: tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; NEXT:
;; 1. Rethink what we are doing: elaborate cl-letf tricks are not
;; necessary.  All we need do is override specific reftex functions.
;; A macro will eventually do all this for us.  So far, there are just
;; three functions to override:
;; - reftex-pop-to-bibtex-entry
;; - reftex-extract-bib-entries
;; - reftex-extract-bib-entries-from-thebibliography
;; while we do something more subtle (too subtle?) with
;; - reftex-parse-from-file
;; 2. Make a minor mode and then make the advice conditional on this
;; mode being active.  [DONE]
;; 3. Check that our scan advice strategy is not fucking up rescans
;; 4. A strategy to check coverage: search reftex codebase for regexps
;; that search for bibtex entries.  These are:
;; For "@\\":
;; reftex-pop-to-bibtex-entry [DONE]
;; reftex-extract-bib-entries [DONE]
;; reftex-get-crossref-alist
;; reftex-parse-bibtex-entry [DONE]
;; reftex-create-bibtex-file
;;
;; For "\\bib":
;; reftex-view-crossref
;; reftex-pop-to-bibtex-entry [DONE]
;; reftex-end-of-bib-entry (called by reftex-view-cr-cite and reftex-pop-to-bibtex-entry)
;; reftex-extract-bib-entries-from-thebibliography [DONE]
;;
;; For reftex-bibliography-commands:
;; reftex-locate-bibliography-files [DONE]

;; 5. Sort out file searching: this is still a mess.  [NO LONGER,
;; PERHAPS]
;; 6. Font-lock \bib entries (just for fun and to learn how to do
;; it).  Extra points for doing something clever with doi and url
;; 7. Think about more translation of fields to bibtex fields: the
;; cite-format stuff could access these.


;; 



;;; Code:

(require 'cl-lib)
(require 'reftex)
(require 'reftex-cite)
(require 'reftex-parse)

;;; Vars

(defvar amsreftex-bib-start-re "\\\\bib[*]?{\\(\\(?:\\w\\|\\s_\\)+\\)}{\\(\\w+\\)}{"
  "Regexp matching start of amsrefs entry.")

;; silence flycheck: this is defined in reftex-parse.
(defvar reftex--index-tags)

;;; File search

;; Searching for files: we setup the ltb file type for
;; reftex-locate-file.  For this, it suffices to setup the following
;; variables:
(defcustom reftex-ltbpath-environment-variables '("TEXINPUTS")
  "List of specifications how to retrieve search path for .ltb database files.
Several entries are possible.
- If an element is the name of an environment variable, its content is used.
- If an element starts with an exclamation mark, it is used as a command
  to retrieve the path.  A typical command with the kpathsearch library would
  be `!kpsewhich -show-path=.tex'.
- Otherwise the element itself is interpreted as a path.
Multiple directories can be separated by the system dependent `path-separator'.
Directories ending in `//' or `!!' will be expanded recursively.
See also `reftex-use-external-file-finders'."
  :group 'reftex-citation-support
  :group 'reftex-finding-files
  :set 'reftex-set-dirty
  :type '(repeat (string :tag "Specification")))

(defvar reftex-ltb-path nil)
;; initialise them
(dolist (prop '(status master-dir recursive-path rec-type))
  (put 'reftex-ltb-path prop nil))
;; and register file extensions and external finders
(add-to-list 'reftex-file-extensions '("ltb"  ".ltb"))
(add-to-list 'reftex-external-file-finders '("ltb" . "kpsewhich %f.ltb"))

;; replacement for reftex-locate-bibliography-files
(defun amsreftex-locate-bibliography-files (master-dir &optional files)
  "Scan buffer for bibliography macros and return file list."
  (unless files
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      (concat 			;TODO: tidy up this regexp
	       "\\(^\\)[^%\n\r]*\\\\\\("
	       "bibselect[*]*"
	       "\\)\\(\\[.+?\\]\\)?{[ \t]*\\([^}]+\\)")
	      nil t)
	(setq files
	      (append files
		      (split-string (reftex-match-string 4)
				    "[ \t\n\r]*,[ \t\n\r]*"))))))
  (when files
    (setq files
          (mapcar
           (lambda (x)
             (reftex-locate-file x "ltb" master-dir))
           files))
    (delq nil files)))

;;; Parsing databases and extracting entries from them

;; We factor out the extraction of fields from \bib entries.
(defun amsreftex-extract-fields (blob &optional prefix)
  "Scan string BLOB for key-value pairs and collect these in an a-list.

Prefix key with string \"PREFIX-\" if PREFIX is non-nil.

Fields with keys 'author' or 'editor' are collected into a single BibTeX-style field."
  (with-temp-buffer
    (fundamental-mode)
    (set-syntax-table reftex-syntax-table-for-bib)
    (insert blob)
    (goto-char (point-min))
    (let (alist start key field authors editors)
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
	  (setq alist (append (amsreftex-extract-fields field key) alist)))
	 ((and (equal key "title") prefix)
	  ;; booktitle comes from compound field
	  (push (cons "booktitle" field) alist)
	  )
	 ((equal key "date")
	  ;; amsrefs has a date field so extract the year
	  (push (cons (concat prefix (when prefix "-") "year")
		      (car (split-string field "-" t)))
		alist))
	 ((equal key "pages")
	  ;; strip amsrefs markup
	  (push (cons (concat prefix (when prefix "-") "pages")
		      (replace-regexp-in-string (regexp-quote "\\ndash ") "-" field))
		alist))
	 (t
	  (push (cons (concat prefix (when prefix "-") key) field) alist))))
      (when authors
	(push (cons (concat prefix (when prefix "-") "author")
		    (mapconcat #'identity  (nreverse authors) " and "))
	      alist))
      (when editors
	(push (cons (concat prefix (when prefix "-") "editor")
		    (mapconcat #'identity  (nreverse editors) " and "))
	      alist))
      (nreverse alist))))

;; Replacement for reftex-parse-bibtex-entry
(defun amsreftex-parse-entry (entry &optional from to)
  "Parse amsrefs ENTRY.
If ENTRY is nil then parse the entry in current buffer between FROM and TO."
  (let (alist thesis-type)
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
	(when (re-search-forward amsreftex-bib-start-re nil t)
	  (setq alist
		(list
		 (cons "&type" (downcase (reftex-match-string 2)))
		 (cons "&key" (reftex-match-string 1))))
	  (setq alist (append alist (amsreftex-extract-fields (buffer-string))))
	  ;; split thesis type into phdthesis and mastersthesis
	  (when (equal "thesis" (amsreftex-get-bib-field "&type" alist))
	    (if (and (setq thesis-type (amsreftex-get-bib-field "type" alist))
		     (string-match "m[.]*s[.]*c\\|master\\|diplom" thesis-type))
		(setf (cdr (assoc "&type" alist)) "mastersthesis")
	      ;; default to phdthesis
	      (setf (cdr (assoc "&type" alist)) "phdthesis")))

	  ;; turn articles into incollection if booktitle present
	  (if (assoc "booktitle" alist)
	      (setf (cdr (assoc "&type" alist)) "incollection"))

	  alist)))))

;; reftex-get-bib-field returns the empty string when the field is not
;; present.  This makes testing for presence more verbose.  So we
;; return nil in this case.
(defun amsreftex-get-bib-field (field entry)
  "Return value of field FIELD in ENTRY or nil if FIELD is not present."
  (cdr (assoc field entry)))


(defun amsreftex--extract-entries (re-list buffer)
  "Extract amsrefs entries that match all regexps in RE-LIST from BUFFER."
  (let (results match-point start-point end-point entry alist
		(first-re (car re-list))
		(re-rest (cdr re-list)))
    (with-current-buffer buffer
      (reftex-with-special-syntax-for-bib
       (save-excursion
	 (goto-char (point-min))
	 (while (re-search-forward first-re nil t)
	   (catch 'search-again
	     (setq match-point (point))
	     (unless (re-search-backward amsreftex-bib-start-re nil t)
	       (throw 'search-again nil))
	     (setq start-point (point))
	     (goto-char (match-end 0))
	     (condition-case nil
		 (up-list 1)
	       (error (goto-char match-point)
		      (throw 'search-again nil)))
	     (setq end-point (point))

	     (when (< end-point match-point) ;match not in entry
	       (goto-char match-point)
	       (throw 'search-again nil))
	     ;; we have an entry so check it against the remaining
	     ;; regexps
	     (setq entry (buffer-substring-no-properties start-point end-point))

	     (dolist (re re-rest)
	       (unless (string-match re entry) (throw 'search-again nil)))

	     (setq alist (amsreftex-parse-entry entry))
	     (push (cons "&entry" entry) alist)
	     ;; TODO crossref stuff?
	     (push (cons "&formatted" (reftex-format-bib-entry alist))
		   alist)
	     (push (amsreftex-get-bib-field "&key" alist) alist)

	     ;; add to the results
	     (push alist results))))))
    (nreverse results)
    ))

;; Replacement for both reftex-extract-bib-entries and
;; reftex-extract-bib-entries-from-thebibliography
(defun amsreftex-extract-entries (buffers)
  "Prompt for regexp and return list of matching entries from BUFFERS.
BUFFERS is a list of buffers or file names."
  (let ((buffer-list (if (listp buffers) buffers (list buffers)))
	re-list
	buffer
	buffer1
	found-list
	default
	first-re)
    ;; Read a regexp, completing on known citation keys.
    (setq default (regexp-quote (reftex-get-bibkey-default)))
    (setq re-list (reftex--query-search-regexps default))

    (if (or (null re-list) (equal re-list '("")))
	(setq re-list (list default)))

    (setq first-re (car re-list))
    (if (string-match "\\`[ \t]*\\'" (or first-re ""))
        (user-error "Empty regular expression"))
    (if (string-match first-re "")
        (user-error "Regular expression matches the empty string"))

    (save-excursion
      (save-window-excursion

        ;; Walk through all database files
        (while buffer-list
          (setq buffer (car buffer-list)
                buffer-list (cdr buffer-list))
          (if (and (bufferp buffer)
                   (buffer-live-p buffer))
              (setq buffer1 buffer)
            (setq buffer1 (reftex-get-file-buffer-force
                           buffer (not reftex-keep-temporary-buffers))))
          (if (not buffer1)
              (message "No such amsrefs database file %s (ignored)" buffer)
            (message "Scanning bibliography database %s" buffer1)
	    (unless (verify-visited-file-modtime buffer1)
              (when (y-or-n-p
                     (format "File %s changed on disk.  Reread from disk? "
                             (file-name-nondirectory
                              (buffer-file-name buffer1))))
                (with-current-buffer buffer1 (revert-buffer t t)))))
	  (setq found-list (append found-list (amsreftex--extract-entries re-list buffer1)))
          
          (reftex-kill-temporary-buffers))))
    (setq found-list (nreverse found-list))

    ;; Sorting
    (cond
     ((eq 'author reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-author))
     ((eq 'year   reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-year))
     ((eq 'reverse-year reftex-sort-bibtex-matches)
      (sort found-list 'reftex-bib-sort-year-reverse))
     (t found-list))
    ))



(defun amsreftex-subvert-reftex-parse-from-file (old-fn file old-docstruct master-dir)
  "Additionally add amsref databases to docstruct of FILE created by OLD-FN.

Intended to advise `reftex-parse-from-file'."
  (let ((docstruct (funcall old-fn file old-docstruct master-dir))
	file-found tmp include-file next-buf buf)
    (catch 'exit
      (setq file-found (reftex-locate-file file "tex" master-dir))
      (if (and (not file-found)
               (setq buf (reftex-get-buffer-visiting file)))
          (setq file-found (buffer-file-name buf)))

      (unless file-found
        (throw 'exit nil))

      (save-excursion

        (message "Scanning file %s" file)
        (set-buffer
         (setq next-buf
               (reftex-get-file-buffer-force
                file-found
                (not (eq t reftex-keep-temporary-buffers)))))

        ;; Begin of file mark
        (setq file (buffer-file-name))
        
        (reftex-with-special-syntax
         (save-excursion
           (save-restriction
             (widen)
             (goto-char 1)
             ;; Find bibliography statement
             (when (setq tmp (amsreftex-locate-bibliography-files master-dir))
               (push (cons 'bib tmp) docstruct))

             (goto-char 1)
             (when (re-search-forward
                    amsreftex-bib-start-re nil t)
               (push (cons 'thebib file) docstruct))

	     ))
	 
	 )))
    ;; Kill the scanned buffer
    (reftex-kill-temporary-buffers next-buf)
    docstruct))

;; Subvert!
(advice-add 'reftex-parse-from-file :around #'amsreftex-subvert-reftex-parse-from-file)

;; Return to status quo ante
;; (advice-remove 'reftex-parse-from-file #'amsreftex-subvert-reftex-parse-from-file )


(defun amsreftex-pop-to-bibtex-entry (key file-list &optional mark-to-kill
					  highlight item return)
  "Find amsrefs KEY in any file in FILE-LIST in another window.
If MARK-TO-KILL is non-nil, mark new buffer to kill.
If HIGHLIGHT is non-nil, highlight the match.
If ITEM in non-nil, search for bibitem instead of database entry.
If RETURN is non-nil, just return the entry and restore point."
  (let* ((re (concat "\\\\bib[*]?{" (regexp-quote key) "}[ \t]*{\\(\\w+\\)}{"))
         (buffer-conf (current-buffer))
         file buf pos oldpos end)

    (catch 'exit
      (while file-list
        (setq file (car file-list)
              file-list (cdr file-list))
        (unless (setq buf (reftex-get-file-buffer-force file mark-to-kill))
          (error "No such file %s" file))
        (set-buffer buf)
	(setq oldpos (point))
        (widen)
        (goto-char (point-min))
        (if (not (re-search-forward re nil t))
	    (goto-char oldpos) ;; restore previous position of point
          (goto-char (match-beginning 0))
          (setq pos (point))
          (when return
            ;; Just return the relevant entry
	    (goto-char (match-end 0))
	    (condition-case nil
		(up-list 1)
	      (error nil))
	    (setq end (point))
	    (setq return (buffer-substring
                          pos end))
	    (goto-char oldpos) ;; restore point.
            (set-buffer buffer-conf)
            (throw 'exit return))
          (switch-to-buffer-other-window buf)
          (goto-char pos)
          (recenter 0)
          (if highlight
              (reftex-highlight 0 (match-beginning 0) (match-end 0)))
          (throw 'exit (selected-window))))
      (set-buffer buffer-conf)
      (error "No amsrefs entry with citation key %s" key))))

(define-minor-mode amsreftex-mode
  "Toggle amsreftex-mode: a minor mode that adjusts `reftex-mode' to use
amsrefs bibliographies rather than BibTeX ones.

This is accomplished by advising those functions of the reftex package
that search or interact with bibliographies.")

(defmacro amsreftex-subvert-fn (old-fn new-fn)
  "Advise OLD-FN so that it is replaced by NEW-FN if `amsreftex-mode' is active."
  (let ((subvert-fn (intern (format "amsreftex-subvert-%s" old-fn))))
    `(progn
       (defun ,subvert-fn (old-fn &rest args)
	 ,(format "If `amsreftex-mode' is active, replace OLD-FN with `%s'.

Intended to advise `%s'" new-fn old-fn)
	 (if amsreftex-mode
	     (apply #',new-fn args)
	   (apply old-fn args)))
       
       (advice-add ',old-fn :around #',subvert-fn))))



(amsreftex-subvert-fn reftex-pop-to-bibtex-entry amsreftex-pop-to-bibtex-entry)





(provide 'amsreftex)
;;; amsreftex.el ends here
