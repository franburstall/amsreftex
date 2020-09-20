# amsreftex

[`amsrefs`](http://www.ams.org/publications/authors/tex/amsrefs)
bibliography support for
[`reftex`](https://www.gnu.org/software/auctex/reftex.html).

# News

New in v0.2: 

* sort your bibliography with `amsreftex-sort-bibliography`.
* Rename `turn-on/off-amsreftex` to `amsreftex-turn-on/off`
  to placate `package-lint`.

# Installation and usage

## Basic usage

Download `amsreftex.el`, put it somewhere in your load-path
and then do
```elisp
(require 'amsreftex)
(amsreftex-turn-on)
```

After this, `reftex` should detect if you are using `amsrefs`
databases and Just Work.

This works by checking for the existence of `\bibselect` or
`\bib` macros.  You may need to re-parse your document with
`M-x reftex-parse-all` after inserting these macros for the
first time.

If, for any reason, you want to revert to vanilla `reftex`,
just do `M-x turn-off-amsreftex`.

## Sorting

Do `M-x amsreftex-sort-bibliography` to sort in-document
bibliographies or stand-alone `amsrefs` databases.

If point is in a `biblist` environment, that alone will be
sorted.  Otherwise, all `\bib` records in the buffer will be
sorted (after checking that you really want to do that).  In
this case, all text outside `\bib` records is left
untouched.

See below to configure the sort order.

# Configuration

## Database search path
By default, `amsreftex` inspects `$TEXINPUTS` to find the
search-path for `amsrefs` databases (`.ltb` files).
Customize `reftex-ltbpath-environment-variables` to change
this.

## Sort order
By default, `amsreftex-sort-bibliography` sorts by author
then year.  The list of fields to sort by is contained in
`amsreftex-sort-fields` so do
```elisp
(setq amsreftex-sort-fields '("author" "title"))
```
to sort by author then title.

You can choose how to sort names by setting
`amsreftex-sort-name-parts`.  By default, we sort by last
name then initial.  Do
```elisp
(setq amsreftex-sort-name-parts '(first last))
```
in the unlikely event that you want to sort by first name
then last name!

# Warnings

1. This is alpha quality software.  It is doubtless riddled
with bugs (raise an issue when you find them) but works for
me.

2. Sorting has a very 1980's ASCII-centric flavour!  Send a
   PR if you can help improve matters.
