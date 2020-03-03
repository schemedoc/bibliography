#! /usr/bin/env gosh

(define (read-all-lines)
  (let loop ((lines '()))
    (let ((line (read-line)))
      (if (eof-object? line) (reverse lines) (loop (cons line lines))))))

(define (bibtex-line->lose-forms line)
  (cond ((rxmatch #/^@([a-z]+)\{([A-Za-z0-9]+),$/ line)
         => (lambda (m)
              `((id ,(rxmatch-substring m 2))
                (type ,(rxmatch-substring m 1)))))
        ((rxmatch #/^\s*([a-z]+)=\{(.*)\},?$/ line)
         => (lambda (m)
              (let ((field (string->symbol (rxmatch-substring m 1)))
                    (value (rxmatch-substring m 2)))
                (case field
                  ((author)
                   (map (lambda (author) `(author ,author))
                        (string-split value " and ")))
                  (else
                   `((,field ,value)))))))
        ((rxmatch #/^\}$/ line)
         '())
        (else
         (error "Cannot parse BibTeX line:" line))))

(define (writeln x) (write x) (newline))

(define (main . args)
  (for-each writeln (append-map bibtex-line->lose-forms (read-all-lines))))
