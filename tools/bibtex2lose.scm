#! /usr/bin/env gosh

(import (scheme base) (scheme write) (gauche base) (srfi 1) (srfi 13))

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
              `((id ,(string->symbol (rxmatch-substring m 2)))
                (type ,(string->symbol (rxmatch-substring m 1))))))
        ((rxmatch #/^\s*([a-z]+)=\{(.*)\},?$/ line)
         => (lambda (m)
              (let ((field (string->symbol
                            (rxmatch-substring m 1)))
                    (value (all-chars->ascii-graphic
                            (rxmatch-substring m 2))))
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

(define (whitespace->spaces str)
  (string-trim-both (regexp-replace-all #/\s+/ str  " ")))

(define (lines->paragraphs lines)
  (define (add para paras)
    (if (null? para) paras
        (let ((para (whitespace->spaces (string-join (reverse para) " "))))
          (if (string-null? para) paras (cons para paras)))))
  (let loop ((lines lines) (paras '()) (para '()))
    (cond ((null? lines)
           (reverse (add para paras)))
          ((string-blank? (car lines))
           (loop (cdr lines) (add para paras) '()))
          (else
           (loop (cdr lines) paras (cons (car lines) para))))))

(define (the-char->string str old-char new-str)
  (regexp-replace-all (regexp-quote (string old-char)) str new-str))

(define (all-chars->ascii-graphic str)
  (set! str (the-char->string str #\x07 "fi"))
  (set! str (the-char->string str #\x0B "ff"))
  (set! str (the-char->string str #\x0C "fi"))
  (set! str (the-char->string str #\x0E "ffi"))
  (set! str (the-char->string str #\x0F "*"))
  (set! str (the-char->string str #\x10 "'"))
  (set! str (the-char->string str #\x13 "'"))
  (set! str (the-char->string str #\x7F ""))
  (set! str (the-char->string str #\x2014 "-"))
  (set! str (the-char->string str #\x2019 "'"))
  (set! str (the-char->string str #\x201C "\""))
  (set! str (the-char->string str #\x201D "\""))
  (set! str (the-char->string str #\x2026 "..."))
  (set! str (regexp-replace-all (regexp-quote "{\\'e}") str "é"))
  (set! str (regexp-replace-all "ob ject" str "object"))
  str)

(define (remove-citations str) (regexp-replace-all #/ \[\d+\]/ str ""))

(define (string-blank? str) (not (not (rxmatch #/^\s*$/ str))))

(define (bibtex-lines+abstract->lose-forms lines)
  (let bibtex ((lines (drop-while string-blank? lines)) (all-forms '()))
    (let ((new-forms (bibtex-line->lose-forms (car lines))))
      (if (not (null? new-forms))
          (bibtex (cdr lines) (append all-forms new-forms))
          (let ((abstract-paragraphs
                 (map (compose remove-citations all-chars->ascii-graphic)
                      (lines->paragraphs (cdr lines)))))
            (for-each assert-ascii abstract-paragraphs)
            (append all-forms
                    `((pdf "")
                      ,@(if (null? abstract-paragraphs) '()
                            `((abstract ,@abstract-paragraphs))))))))))

(define (writeln x) (write x) (newline))

(define (main)
  (newline)
  (for-each writeln (bibtex-lines+abstract->lose-forms (read-all-lines))))

(main)
