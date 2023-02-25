#! /usr/bin/env gosh

;;;; Script to convert BibTeX bibliography to losebib

;;; Copyright 2020-2021 Lassi Kortela
;;; SPDX-License-Identifier: MIT

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

(define (guess-url-type mappings url)
  (car (fold (lambda (mapping winner)
               (let* ((win-sym (car winner))
                      (win-pos (cdr winner))
                      (cur-sym (car mapping))
                      (cur-ext (cdr mapping))
                      (cur-pos (string-contains-ci url cur-ext)))
                 (if (and cur-pos (or (not win-pos) (< win-pos cur-pos)))
                     (cons cur-sym cur-pos)
                     winner)))
             (cons #f #f)
             mappings)))

(define (maybe-urls urls)
  (let ((mappings '((pdf . "pdf") (ps . "ps") (html . "html"))))
    (let loop ((urls urls) (by-type (map list (map car mappings))))
      (if (null? urls)
          (append-map (lambda (by-this)
                        (if (null? (cdr by-this)) '()
                            (map (lambda (url) (list (car by-this) url))
                                 (cdr by-this))))
                      by-type)
          (let* ((url (car urls))
                 (url-type (or (guess-url-type mappings url)
                               (error "Unknown URL type:" url))))
            (let ((by-this (assoc url-type by-type)))
              (set-cdr! by-this (append (cdr by-this) (list url))))
            (loop (cdr urls) by-type))))))

(define (bibtex lines urls all-forms)
  (let ((new-forms (bibtex-line->lose-forms (car lines))))
    (if (not (null? new-forms))
        (bibtex (cdr lines) urls (append all-forms new-forms))
        (let ((abstract-paragraphs
               (map (compose remove-citations all-chars->ascii-graphic)
                    (lines->paragraphs (cdr lines)))))
          ;;(for-each assert-ascii abstract-paragraphs)
          (append all-forms
                  (maybe-urls urls)
                  (if (null? abstract-paragraphs) '()
                      `((abstract ,@abstract-paragraphs))))))))

(define (bibtex-lines+abstract->lose-forms lines)
  (let prelude ((lines lines) (urls '()))
    (cond ((null? lines)
           '())
          ((string-blank? (car lines))
           (prelude (cdr lines) urls))
          ((string-contains (car lines) "://")
           (prelude (cdr lines) (cons (string-trim-both (car lines)) urls)))
          (else
           (bibtex lines urls '())))))

(define (writeln x) (write x) (newline))

(define (main)
  (newline)
  (for-each writeln (bibtex-lines+abstract->lose-forms (read-all-lines))))

(main)
