;;; amsreftex.el --- Library to enable reftex to use amsrefs bibliographies  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Fran Burstall

;; Author: Fran Burstall <fran.burstall@gmail.com>
;; Version: 0.1
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


;; 4. A strategy to check coverage: search reftex codebase for regexps
;; that search for bibtex entries.  These are:
;; For "@\\":
;; reftex-pop-to-bibtex-entry [DONE]
;; reftex-extract-bib-entries [DONE]
;; reftex-get-crossref-alist [DONE: no need to subvert]
;; reftex-parse-bibtex-entry [DONE]
;; reftex-create-bibtex-file [IGNORED]
;;
;; For "\\bib":
;; reftex-view-crossref.  Q: what does this do on a bibitem? A: show a \cite.
;; reftex-pop-to-bibtex-entry [DONE]
;; reftex-end-of-bib-entry (called by reftex-view-cr-cite (to get a
;; tmp window height) and reftex-pop-to-bibtex-entry)
;; reftex-extract-bib-entries-from-thebibliography [DONE]
;;
;; For reftex-bibliography-commands:
;; reftex-locate-bibliography-files [DONE]

;; TO DO:

;; 1.  Write a commentary:
;;     - Installation
;;     - Implementation
;; 2.  Package for GitHub
;;     - README.md
;;     - tidy up the repo
;; 3.  Package for MELPA
;;     - Package-Requires
;;     - URL
;;     - lots of linting
;; 4.  Think about more translation of fields to bibtex fields: the
;; cite-format stuff could access these.




;; NEXT:
;; (a) Look into better formatting of \bib by auctex.  Best option is
;; no formatting.  Should add an entry to
;; LaTeX-indent-environment-list.  Learn about how filling works...
;; 
;; 


;; 



;;; Code:

(require 'cl-lib)
(require 'reftex)
(require 'reftex-cite)
(require 'reftex-parse)
(require 'reftex-dcr)

;;; Vars

(defvar amsreftex-bib-start-re "\\(\\\\bib[*]?\\){\\(\\(?:\\w\\|\\s_\\)+\\)}{\\(\\w+\\)}{"
  "Regexp matching start of amsrefs entry.")

(defvar amsreftex-kv-start-re "\\(\\(?:\\w\\|-\\)+\\)[ \t\n\r]*=[ \t\n\r]*{"
  "Regexp matching start of key-val pair in amsrefs entry.")

;; silence flycheck: this is defined in reftex-parse.
(defvar reftex--index-tags)

;; Fontification
(defvar amsreftex-font-lock-keywords
  `(
    (,amsreftex-bib-start-re (1 font-lock-keyword-face) (2 font-lock-type-face) (3 font-lock-function-name-face))
    (,amsreftex-kv-start-re (1 font-lock-variable-name-face))))

;; whether we are active
(defvar amsreftex-p nil
  "Non-nil if amsreftex is active.")

;;; Files and file search

;; amsrefs uses .ltb files for its databases.  These are essentially
;; LaTeX files so treat them as such:
(cl-pushnew '("\\.ltb\\'" . latex-mode) auto-mode-alist :test 'equal)

;; and add some fontification for \bib macros
(font-lock-add-keywords 'latex-mode amsreftex-font-lock-keywords)

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
  "Scan buffer for bibliography macros and return file list.

Use MASTER-DIR as root for relative paths during file-search.

If FILES is present, list these instead."
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
      (while (re-search-forward amsreftex-kv-start-re nil t)
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
		 (cons "&type" (downcase (reftex-match-string 3)))
		 (cons "&key" (reftex-match-string 2))))
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
	     ;; look for start of \bib, taking care since the match
	     ;; could be the cite-key and so be missing by
	     ;; re-search-backward on amsreftex-bib-start-re
	     (unless (re-search-backward  "^[^%\n]*?\\\\bib[ \t]*{"  nil t)
	       (throw 'search-again nil))
	     (setq start-point (point))
	     ;; go to end of first line of entry
	     (re-search-forward amsreftex-bib-start-re nil t)
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
	     ;; crossref stuff
	     (if (assoc "xref" alist)
                 (setq alist
                       (append
                        alist (amsreftex-get-crossref-alist alist))))
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

(defun amsreftex-get-crossref-alist (entry)
  "Return the alist from a crossref ENTRY."
  (let ((crkey (cdr (assoc "xref" entry)))
        start)
    (save-excursion
      (save-restriction
        (widen)
        (if (re-search-forward
             (concat "^[^%\n]*?\\(\\\\bib\\*\\)[ \t]*{"
		     (regexp-quote crkey)
		     "}[ \t]*{\\w+}[ \t]*{")
	     nil t)
            (progn
              (setq start (match-beginning 1))
              (condition-case nil
                  (up-list 1)
                (error nil))
              (amsreftex-extract-fields
	       (buffer-substring-no-properties start (point))
	       "book"))
          nil)))))

;;; Parsing the source file

(defun amsreftex-using-amsrefs-p ()
  "Return non-nil if we seem to be using amsrefs databases."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward
     (concat
      "\\(^[^%\n]*?\\\\bibselect\\|"
      amsreftex-bib-start-re "\\)")
     nil t)))

;; reftex-parse-from-file is a big function but we only have to change
;; a tiny bit of it (see comment therein) to make it amsrefs-friendly.

;; Replacement for reftex-parse-from-file
(defun amsreftex-parse-from-file (file docstruct master-dir)
  "Scan the buffer of FILE for labels and save them in a list DOCSTRUCT.

Use MASTER-DIR as root for relative paths during file-search.

Additionally add amsref databases."
  (let ((regexp (reftex-everything-regexp))
        (bound 0)
        file-found tmp include-file
        (level 1)
        (highest-level 100)
        toc-entry index-entry next-buf buf)

    (catch 'exit
      (setq file-found (reftex-locate-file file "tex" master-dir))
      (if (and (not file-found)
               (setq buf (reftex-get-buffer-visiting file)))
          (setq file-found (buffer-file-name buf)))

      (unless file-found
        (push (list 'file-error file) docstruct)
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
        (push (list 'bof file) docstruct)

        (reftex-with-special-syntax
         (save-excursion
           (save-restriction
             (widen)
             (goto-char 1)

             (while (re-search-forward regexp nil t)

               (cond

                ((match-end 1)
                 ;; It is a label
		 (when (or (null reftex-label-ignored-macros-and-environments)
			   ;; \label{} defs should always be honored,
			   ;; just no keyval style [label=foo] defs.
			   (string-equal "\\label{" (substring (reftex-match-string 0) 0 7))
                           (if (and (fboundp 'TeX-current-macro)
                                    (fboundp 'LaTeX-current-environment))
                               (not (or (member (save-match-data (TeX-current-macro))
                                                reftex-label-ignored-macros-and-environments)
                                        (member (save-match-data (LaTeX-current-environment))
                                                reftex-label-ignored-macros-and-environments)))
                             t))
		   (push (reftex-label-info (reftex-match-string 1) file bound)
			 docstruct)))

                ((match-end 3)
                 ;; It is a section

		 ;; Use the beginning as bound and not the end
		 ;; (i.e. (point)) because the section command might
		 ;; be the start of the current environment to be
		 ;; found by `reftex-label-info'.
                 (setq bound (match-beginning 0))
		 ;; The section regexp matches a character at the end
		 ;; we are not interested in.  Especially if it is the
		 ;; backslash of a following macro we want to find in
		 ;; the next parsing iteration.
		 (when (eq (char-before) ?\\) (backward-char))
                 ;; Insert in List
                 (setq toc-entry (funcall reftex-section-info-function file))
                 (when (and toc-entry
                            (eq ;; Either both are t or both are nil.
                             (= (char-after bound) ?%)
                             (string-suffix-p ".dtx" file)))
                   ;; It can happen that section info returns nil
                   (setq level (nth 5 toc-entry))
                   (setq highest-level (min highest-level level))
                   (if (= level highest-level)
                       (message
                        "Scanning %s %s ..."
                        (car (rassoc level reftex-section-levels-all))
                        (nth 6 toc-entry)))

                   (push toc-entry docstruct)
                   (setq reftex-active-toc toc-entry)))

                ((match-end 7)
                 ;; It's an include or input
                 (setq include-file (reftex-match-string 7))
                 ;; Test if this file should be ignored
                 (unless (delq nil (mapcar
                                    (lambda (x) (string-match x include-file))
                                    reftex-no-include-regexps))
                   ;; Parse it
                   (setq docstruct
                         (reftex-parse-from-file
                          include-file
                          docstruct master-dir))))

                ((match-end 9)
                 ;; Appendix starts here
                 (reftex-init-section-numbers nil t)
                 (push (cons 'appendix t) docstruct))

                ((match-end 10)
                 ;; Index entry
                 (when reftex-support-index
                   (setq index-entry (reftex-index-info file))
                   (when index-entry
                     (cl-pushnew (nth 1 index-entry) reftex--index-tags :test #'equal)
                     (push index-entry docstruct))))

                ((match-end 11)
                 ;; A macro with label
                 (save-excursion
                   (let* ((mac (reftex-match-string 11))
                          (label (progn (goto-char (match-end 11))
                                        (save-match-data
                                          (reftex-no-props
                                           (reftex-nth-arg-wrapper
                                            mac)))))
                          (typekey (nth 1 (assoc mac reftex-env-or-mac-alist)))
                          (entry (progn (if typekey
                                            ;; A typing macro
                                            (goto-char (match-end 0))
                                          ;; A neutral macro
                                          (goto-char (match-end 11))
                                          (reftex-move-over-touching-args))
                                        (reftex-label-info
                                         label file bound nil nil))))
                     (push entry docstruct))))
                (t (error "This should not happen (reftex-parse-from-file)")))
               )

	     ;; amsreftex changes start here
	     (cond
	      ((amsreftex-using-amsrefs-p)
	       (push (cons 'database "amsrefs") docstruct)
	       ;; Find amsrefs bibliography statement
	       (when (setq tmp (amsreftex-locate-bibliography-files master-dir))
		 (push (cons 'bib tmp) docstruct)))
	      (t
               ;; Find bib(la)tex bibliography statement
               (when (setq tmp (reftex-locate-bibliography-files master-dir))
		 (push (cons 'bib tmp) docstruct))))

             (goto-char 1)
             (when (re-search-forward
                    (concat  "\\(\\(\\`\\|[\n\r]\\)[ \t]*\\\\begin{thebibliography}\\)\\|\\("
			     amsreftex-bib-start-re
			     "\\)")
		    nil t)
               (push (cons 'thebib file) docstruct))
	     ;; amsreftex changes end here

	     ;; Find external document specifications
             (goto-char 1)
             (while (re-search-forward "[\n\r][ \t]*\\\\externaldocument\\(\\[\\([^]]*\\)\\]\\)?{\\([^}]+\\)}" nil t)
               (push (list 'xr-doc (reftex-match-string 2)
                           (reftex-match-string 3))
                     docstruct))

             ;; End of file mark
             (push (list 'eof file) docstruct)))))

      ;; Kill the scanned buffer
      (reftex-kill-temporary-buffers next-buf))

    ;; Return the list
    docstruct))

 


;;; Pop to entry and echo citations

;; Replace the callback...the issue here is that this callback may be
;; called in a context (the Ref-Select buffer) where we have no access
;; to the reftex-docstruct-symbol of the document buffer.  This means
;; our usual advice method applied to reftex-pop-to-bibtex-entry may
;; not apply so we must test for amsrefs in a different way (by
;; inpsecting the format of the entry) and manually choose the
;; function for popping to the entry.

(defun amsreftex-database-selection-callback (data _ignore no-revisit)
  "Display database entry corresponding to DATA.

If NO-REVISIT is non-nil, only search existing buffers.

Callback function to be called from the Reftex-Select selection, in
order to display context.  This function is relatively slow and not
recommended for follow mode.  It works OK for individual lookups.

This version also tests whether amsrefs databases are in use and
dispatches the pop-to-entry function based on that."
  (let ((win (selected-window))
        (key (reftex-get-bib-field "&key" data))
	(amsrefs (string-match amsreftex-bib-start-re
			       (reftex-get-bib-field "&entry" data)))
        bibfile-list item bibtype pop-fn)

    (setq pop-fn (if amsrefs
		     #'amsreftex-pop-to-database-entry
		   #'reftex-pop-to-bibtex-entry))

    (catch 'exit
      (with-current-buffer reftex-call-back-to-this-buffer
        (setq bibtype (reftex-bib-or-thebib))
        (cond
         ((eq bibtype 'bib)
					;        ((assq 'bib (symbol-value reftex-docstruct-symbol))
          (setq bibfile-list (reftex-get-bibfile-list)))
         ((eq bibtype 'thebib)
					;        ((assq 'thebib (symbol-value reftex-docstruct-symbol))
          (setq bibfile-list
                (reftex-uniquify
                 (mapcar 'cdr
                         (reftex-all-assq
                          'thebib (symbol-value reftex-docstruct-symbol))))
                item t))
         (reftex-default-bibliography
          (setq bibfile-list (reftex-default-bibliography)))
         (t (ding) (throw 'exit nil))))

      (when no-revisit
        (setq bibfile-list (reftex-visited-files bibfile-list)))

      (condition-case nil
          (funcall pop-fn
		   key bibfile-list (not reftex-keep-temporary-buffers) t item)
        (error (ding))))

    (select-window win)))

;; Replacement for reftex-pop-to-bibtex-entry
(defun amsreftex-pop-to-database-entry (key file-list &optional mark-to-kill
					    highlight _ return)
  "Find amsrefs KEY in any file in FILE-LIST in another window.
If MARK-TO-KILL is non-nil, mark new buffer to kill.
If HIGHLIGHT is non-nil, highlight the match.
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
	    (goto-char (1- (match-end 0)))
	    (setq end (reftex-end-of-bib-entry nil)) ; does forward-list
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


;;* Subvert relevant reftex functions

;; Silence the byte-compiler: we shall define these functions by macro below.
(declare-function amsreftex-subvert-reftex-locate-bibliography-files "amsreftex" t)
(declare-function amsreftex-subvert-reftex-parse-bibtex-entry "amsreftex" t)
(declare-function amsreftex-subvert-reftex-get-crossref-alist "amsreftex" t)
(declare-function amsreftex-subvert-reftex-extract-bib-entries "amsreftex"  t)
(declare-function amsreftex-subvert-reftex-extract-bib-entries-from-thebibliography "amsreftex" t)
(declare-function amsreftex-subvert-reftex-pop-to-bibtex-entry "amsreftex" t)

(defmacro amsreftex-subvert-fn (old-fn new-fn)
  "Advise OLD-FN so that it is replaced by NEW-FN if `amsreftex-mode' is active."
  (let ((subvert-fn (intern (format "amsreftex-subvert-%s" old-fn))))
    `(progn
       (defun ,subvert-fn (old-fn &rest args)
	 ,(format "If amsrefs databases are in use,, replace OLD-FN with `%s'.

Intended to advise `%s'" new-fn old-fn)
	 (unless (symbol-value reftex-docstruct-symbol)
	   (error "Advised fn %S called in bad context" old-fn ))
	 (if (assq 'database (symbol-value reftex-docstruct-symbol))
	     (apply #',new-fn args)
	   (apply old-fn args)))
       
       (advice-add ',old-fn :around #',subvert-fn))))

(defun amsreftex-set-last-arg-to-nil (args)
  "If amsrefs databases are in use, set last element of ARGS to nil."
  (when (assq 'database (symbol-value reftex-docstruct-symbol))
    (setf (car (last args)) nil))
  args)
;;;###autoload
(defun turn-on-amsreftex ()
  "Turn on amsreftex.

This advises several reftex functions to make them work with masrefs databases."
  (interactive)
  ;; conditionally replace these fns with their amsreftex versions
  (amsreftex-subvert-fn reftex-locate-bibliography-files amsreftex-locate-bibliography-files)
  (amsreftex-subvert-fn reftex-parse-bibtex-entry amsreftex-parse-entry)
  (amsreftex-subvert-fn reftex-get-crossref-alist amsreftex-get-crossref-alist)
  (amsreftex-subvert-fn reftex-extract-bib-entries amsreftex-extract-entries)
  (amsreftex-subvert-fn reftex-extract-bib-entries-from-thebibliography amsreftex-extract-entries)
  (amsreftex-subvert-fn reftex-pop-to-bibtex-entry amsreftex-pop-to-database-entry)
  ;; Both reftex-echo-cite and reftex-end-of-bib-entry have a last
  ;; argument ITEM for dealing with the case of on-board \bibitems.  We
  ;; set this argument to nil.
  (advice-add 'reftex-echo-cite :filter-args #'amsreftex-set-last-arg-to-nil)
  (advice-add 'reftex-end-of-bib-entry :filter-args #'amsreftex-set-last-arg-to-nil)
  ;; unconditionally replace two functions:
  ;; 1. Replace reftex-parse-from-file just to get off the ground.
  ;; This is what makes document buffers amsrefs-aware.
  ;; 2. reftex-bibtex-selection-callback is called from a buffer that
  ;; is not amsrefs-aware.
  (advice-add 'reftex-parse-from-file :override #'amsreftex-parse-from-file)
  (advice-add 'reftex-bibtex-selection-callback :override #'amsreftex-database-selection-callback)
  (setq amsreftex-p t))

(defun turn-off-amsreftex ()
  "Turn off amsreftex.

 We remove all advice added by `turn-on-amsrefs'."
  (interactive)
  (if (not amsreftex-p)
      (user-error "Amsreftex is not turned on!")
    (advice-remove 'reftex-locate-bibliography-files #'amsreftex-subvert-reftex-locate-bibliography-files)
    (advice-remove 'reftex-parse-bibtex-entry #'amsreftex-subvert-reftex-parse-bibtex-entry)
    (advice-remove 'reftex-get-crossref-alist #amsreftex-subvert-reftex-get-crossref-alist)
    (advice-remove 'reftex-extract-bib-entries #'amsreftex-subvert-reftex-extract-bib-entries)
    (advice-remove 'reftex-extract-bib-entries-from-thebibliography
		   #'amsreftex-subvert-reftex-extract-bib-entries-from-thebibliography)
    (advice-remove 'reftex-pop-to-bibtex-entry #'amsreftex-subvert-reftex-pop-to-bibtex-entry)
    (advice-remove 'reftex-echo-cite #'amsreftex-set-last-arg-to-nil)
    (advice-remove 'reftex-end-of-bib-entry  #'amsreftex-set-last-arg-to-nil)
    (advice-remove 'reftex-parse-from-file  #'amsreftex-parse-from-file)
    (advice-remove 'reftex-bibtex-selection-callback  #'amsreftex-database-selection-callback))
  )

(provide 'amsreftex)


;;; amsreftex.el ends here
