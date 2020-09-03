# amsreftex

[`amsrefs`](http://www.ams.org/publications/authors/tex/amsrefs)
bibliography support for
[`reftex`](https://www.gnu.org/software/auctex/reftex.html).

# Installation and usage

Download `amsreftex.el`, put it somewhere in your load-path
and then do
```elisp
(require 'amsreftex)
(turn-on-amsreftex)
```

After this, `reftex` should detect if you are using `amsrefs`
databases and Just Work.

This works by checking for the existence of `\bibselect` or
`\bib` macros.  You may need to re-parse your document with
`M-x reftex-parse-all` after inserting these macros for the
first time.

If, for any reason, you want to revert to vanilla `reftex`,
just do `M-x turn-off-amsreftex`.

# Configuration

There is almost nothing to configure.  The one exception: by
default, `amsreftex` inspects `$TEXINPUTS` to find the
search-path for `amsrefs` databases (`.ltb` files).
Customize `reftex-ltbpath-environment-variables` to change this.


# Warning

This is alpha quality software.  It is doubtless riddled
with bugs (raise an issue when you find them) but works for
me.
