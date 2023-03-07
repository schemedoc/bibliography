# Bibliography Format Specification

This document specifies the format of `.scm` files for bibliographic database
used in this repository.

The overview of format is:

    (group <description of group>)

    (id <entry id>)
    (type <BibTeX entry type>)
    (title <title>)
    (author <author name>)
    [(author <author name>) ...]
    [(year <published year>)]
    [(month <published month>)]
    [(number <report number>)]
    [(institution <institute name>)]
    [(booktitle <book title>)]
    [... other BibTeX/BibLaTeX fields ...]
    [(pdf <URL to a PDF file>)]
    [(pdf-sha1 <SHA-1 fingerprint of the PDF file>)]
    [(ps <URL to a PostScript file>)]
    [(ps-sha1 <SHA-1 fingerprint of the PDF file>)]

    [(id ...) ...]

    (end-group)

## Name Formats

If the `<name>` in `(author <name>)` or `(editor <name>)` is a string,
its interpretation is unspecified.

If the `<name>` is `(bibtex <given> <particle> <family> <suffix>)`
where `<given>`, `<particle>`, `<family>` and `<suffix>` are strings,
it is interpreted as a *BibTeX-modeled name.*

In the BibTeX-modeled names, `<given>`, `<particle>` and `<suffix>` can be
zero-length strings.

It is an error if the `<name>` is neither of the above.
