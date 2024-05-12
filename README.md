# amsreftex
[![MELPA](https://melpa.org/packages/amsreftex-badge.svg)](https://melpa.org/#/amsreftex)

[`amsrefs`](http://www.ams.org/publications/authors/tex/amsrefs)
bibliography support for
[`reftex`](https://www.gnu.org/software/auctex/reftex.html).

# News

## v0.3.2

Restore fontlock for bibliographies under auctex version >= 14.0.

## v0.3.1

Tiny changes to some doc-strings to calm the byte-compiler.

## v0.3

Invisible (I hope) changes to silence the byte-compiler.

## v0.2: 

* sort your bibliography with `amsreftex-sort-bibliography`.
* Rename `turn-on/off-amsreftex` to `amsreftex-turn-on/off`
  to placate `package-lint`.

# Installation and activation

## From MELPA
```elisp
(install-package 'amsreftex)
```
and activate with
```elisp
(amsreftex-turn-on)
```
or do it in one shot with
```elisp
(use-package amsreftex
  :ensure t
  :config
  (amsreftex-turn-on))
```
## Manual
Download `amsreftex.el`, put it somewhere in your load-path
and then do
```elisp
(require 'amsreftex)
(amsreftex-turn-on)
```
# Usage

## Basic usage
Once `amsreftex` is activated, `reftex` should detect if you are using `amsrefs`
databases and Just Work.

It does this by checking for the existence of `\bibselect` or
`\bib` macros.  You may need to re-parse your document with
`M-x reftex-parse-all` after inserting these macros for the
first time.

If, for any reason, you want to revert to vanilla `reftex`,
just do `M-x amsreftex-turn-off`.

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
