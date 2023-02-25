;;;; Library for reading losebib files

;;; Copyright 2020 Lassi Kortela
;;; SPDX-License-Identifier: MIT

(define-library (losebib-read)
  (export losebib-read-all losebib-read-file)
  (import (scheme base) (scheme file) (scheme read))
  (begin

    (define (read-group-loop form group)
      (cond ((eof-object? form)
             (error "Missing end-group"))
            ((equal? 'end-group (car form))
             (unless (null? (cdr form)) (error "end-group: usage"))
             (reverse group))
            ((equal? 'group (car form))
             (let ((inner-group (read-group form)))
               (read-group-loop (read) (cons inner-group group))))
            ((equal? 'id (car form))
             (let read-entry ((form (read)) (entry (list form)))
               (if (or (eof-object? form)
                       (member (car form) '(id group end-group)))
                   (read-group-loop form (cons (reverse entry) group))
                   (read-entry (read) (cons form entry)))))
            (else
             (error "Expecting (id ...)"))))

    (define (read-group form)
      (unless (= 1 (length (cdr form))) (error "group: usage"))
      (let ((group-name (cadr form)))
        (read-group-loop (read) (list group-name))))

    (define (losebib-read-all)
      (let ((top-group (read-group (read))))
        (unless (eof-object? (read)) (error "Things after end-group"))
        top-group))

    (define (losebib-read-file filename)
      (with-input-from-file filename losebib-read-all))))
