#! /usr/bin/env gosh

(import (scheme base) (scheme write) (gauche base) (srfi 13))

(define (read-all-lines)
  (let loop ((lines '()))
    (let ((line (read-line)))
      (if (eof-object? line) (reverse lines) (loop (cons line lines))))))

(define (assert-ascii string)
  (define (ascii-non-control? char) (char<=? #\space char #\~))
  (string-for-each (lambda (char)
                     (unless (ascii-non-control? char)
                       (error "Non-ASCII character:" char)))
                   string))

(define (bibtex-line->lose-forms line)
  (cond ((rxmatch #/^@([a-z]+)\{([A-Za-z0-9]+),$/ line)
         => (lambda (m)
              `((id ,(rxmatch-substring m 2))
                (type ,(rxmatch-substring m 1)))))
        ((rxmatch #/^\s*([a-z]+)=\{(.*)\},?$/ line)
         => (lambda (m)
              (let ((field (string->symbol (rxmatch-substring m 1)))
                    (value (rxmatch-substring m 2)))
                (assert-ascii value)
                (case field
                  ((author)
                   (map (lambda (author) `(author ,author))
                        (string-split value " and ")))
                  ((year)
                   `((year ,(string->number value))))
                  (else
                   `((,field ,value)))))))
        ((rxmatch #/^\s*\}$/ line)
         '())
        (else
         (error "Cannot parse BibTeX line:" line))))

(define (all-whitespace->spaces string)
  (string-trim-both (regexp-replace-all #/\s+/ string " ")))

(define (the-char->string str old-char new-str)
  (regexp-replace-all (regexp-quote (string old-char)) str new-str))

(define (all-chars->ascii-graphic str)
  (set! str (the-char->string str #\x0C "fi"))
  (set! str (the-char->string str #\x2019 "'"))
  (assert-ascii str)
  str)

(define (bibtex-lines+abstract->lose-forms lines)
  (let bibtex ((lines lines) (all-forms '()))
    (let ((new-forms (bibtex-line->lose-forms (car lines))))
      (if (not (null? new-forms))
          (bibtex (cdr lines) (append all-forms new-forms))
          (let ((abstract
                 (all-chars->ascii-graphic
                  (all-whitespace->spaces
                   (string-join (cdr lines) " ")))))
            (append all-forms
                    `((pdf "")
                      ,@(if (string-null? abstract) '()
                            `((abstract ,abstract))))))))))

(define (writeln x) (write x) (newline))

(define (main)
  (newline)
  (for-each writeln (bibtex-lines+abstract->lose-forms (read-all-lines))))

(main)
