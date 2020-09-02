# amsreftex

A library that extends
[`reftex`](https://www.gnu.org/software/auctex/reftex.html)
to allow the use of 
[`amsrefs`](http://www.ams.org/publications/authors/tex/amsrefs)
databases.

# Installation and usage

Download `amsreftex.el`, put it somewhere in your load-path
and then do
```elisp
(require 'amsreftex)
```

Once loaded, `reftex` should detect if you are using `amsrefs`
databases and Just Work.

This works by checking for the existence of `\bibselect` or
`\bib` macros.  You may need to re-parse your documet with
`M-x reftex-parse-all` after inserting these macros for the
first time.

# Warning

This is alpha quality software.  It is doubtless riddled
with bugs (raise an issue when you find them) but works for
me.
