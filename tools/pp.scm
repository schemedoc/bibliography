#!

(import (scheme base) (scheme read) (scheme write))

(define (read-all-entries)
  (let read-entries ((form (read)) (entries '()))
    (cond ((eof-object? form) (reverse entries))
          ((equal? 'id (car form))
           (let read-entry ((form (read)) (entry (list form)))
             (if (or (eof-object? form) (equal? 'id (car form)))
                 (read-entries form (cons (reverse entry) entries))
                 (read-entry (read) (cons form entry)))))
          (else (error "File does not start with (id ...)")))))

(define (writeln x) (write x) (newline))

(for-each (lambda (entry) (for-each writeln entry) (newline))
          (read-all-entries))
