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
