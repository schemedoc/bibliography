#!

(import (scheme base) (scheme read) (scheme write))

(define (disp . xs) (for-each display xs) (newline))

(define (writeln x) (write x) (newline))

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
      (else (error "Assoc:" key)))))

(define (assoc1 key alist)
  (let ((all (assoc* key alist)))
    (unless (= 1 (length all)) (error "Assoc:" key))
    (car all)))

(define (filter f xs)
  (let loop ((xs xs) (acc '()))
    (if (null? xs) (reverse acc)
        (loop (cdr xs) (if (f (car xs)) (cons (car xs) acc) acc)))))

(define (nonemp . xs)
  (filter (lambda (x) (and x (not (equal? "" x))))
          xs))

(define (string-interpose sep strings)
  (if (null? strings) '()
      (let loop ((acc (car strings)) (strings (cdr strings)))
        (if (null? strings) acc
            (loop (string-append acc sep (car strings)) (cdr strings))))))

;;;

(define (month-name month)
  (case month
    ((1) "January")
    ((2) "February")
    ((3) "March")
    ((4) "April")
    ((5) "May")
    ((6) "June")
    ((7) "July")
    ((8) "August")
    ((9) "September")
    ((10) "October")
    ((11) "November")
    ((12) "December")
    (else (error "Bad month:" month))))

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
           (if (or (eof-object? form) (member (car form) '(id group end-group)))
               (read-group-loop form (cons (reverse entry) group))
               (read-entry (read) (cons form entry)))))
        (else
         (error "Expecting (id ...)"))))

(define (read-group form)
  (unless (= 1 (length (cdr form))) (error "group: usage"))
  (let ((group-name (cadr form)))
    (read-group-loop (read) (list group-name))))

(define (read-all-entries)
  (let ((top-group (read-group (read))))
    (unless (eof-object? (read)) (error "Things after end-group"))
    top-group))

(define (italic text) (string-append "_" text "_"))
(define (quoted text) (string-append "\"" text "\""))
(define (linkto url text) (string-append "[" text "](" url ")"))

(define (markdown entry)
  (string-append
   (string-interpose
    ". " (nonemp
          (string-interpose " and " (assoc* 'author entry))
          (quoted (assoc1 'title entry))
          (let ((book (assoc? 'booktitle entry)))
            (and book (italic book)))
          (string-interpose
           " " (nonemp (let ((month (assoc? 'month entry)))
                         (and month (month-name month)))
                       (let ((year (assoc? 'year entry)))
                         (and year (number->string year)))))
          (let ((links (append (map (lambda (url) (linkto url "pdf"))
                                    (assoc* 'pdf entry))
                               (map (lambda (url) (linkto url "ps"))
                                    (assoc* 'ps entry)))))
            (and links
                 (string-append "Available online: "
                                (string-interpose " " links))))))
   "\n"))

(define (display-entries-as-markdown entries)
  (display (string-interpose "\n" (map markdown entries))))

(define (redisplay-entries-as-lose entries)
  (for-each (lambda (entry) (for-each writeln entry) (newline))
            entries))

;;(display-entries-as-markdown (read-all-entries))
;;(redisplay-entries-as-lose (read-all-entries))
(write (read-all-entries)) (newline)
