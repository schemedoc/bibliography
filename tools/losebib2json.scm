#!/bin/sh
#|
exec gosh -I$(dirname $0) -- $0 "$@"
|#
;;;; Script to convert losebib file to JSON

;;; Copyright 2020 Lassi Kortela
;;; SPDX-License-Identifier: MIT

(import (scheme base) (scheme write) (srfi 1))
(import (rfc json))
(import (losebib-read))

(define (disp . xs) (for-each display xs) (newline))

(define (assoc* key alist)
  (let loop ((alist alist) (acc '()))
    (if (null? alist) acc
        (loop (cdr alist)
              (if (equal? key (caar alist))
                  (append acc (cdar alist))
                  acc)))))

(define (assoc? key alist)
  (let ((all (assoc* key alist)))
    (case (length all)
      ((0) #f)
      ((1) (car all))
      (else (error "Assoc:" key alist)))))

(define (assoc1 key alist)
  (let ((all (assoc* key alist)))
    (unless (= 1 (length all)) (error "Assoc:" key alist))
    (car all)))

(define (losebib->json entry)
  `((id . ,(symbol->string (assoc1 'id entry)))
    (bibtex_type . ,(symbol->string (assoc1 'type entry)))
    (title . ,(assoc1 'title entry))
    (authors . ,(list->vector (assoc* 'author entry)))
    (booktitle . ,(assoc? 'booktitle entry))
    (volume . ,(assoc? 'volume entry))
    (number . ,(assoc? 'number entry))
    (year . ,(assoc? 'year entry))
    (month . ,(assoc? 'month entry))
    (publisher . ,(assoc? 'publisher entry))
    (pdf_urls . ,(list->vector (assoc* 'pdf entry)))
    (abstract . ,(list->vector (assoc* 'abstract entry)))))

(define (convert x)
  (if (string? (car x))
      (append-map convert (cdr x))
      (list (losebib->json x))))

(construct-json (list->vector (convert (losebib-read-all))))
