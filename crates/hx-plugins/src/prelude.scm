;; hx plugin prelude
;; Standard definitions and utilities for hx plugins

;; Command registry - stores registered commands
(define *hx-commands* (make-hash))

;; Define a custom command
;; Usage: (define-command "name" #:description "desc" handler)
(define-syntax define-command
  (syntax-rules ()
    [(_ name handler)
     (hash-set! *hx-commands* name
       (hash 'name name
             'description ""
             'usage ""
             'handler handler))]
    [(_ name #:description desc handler)
     (hash-set! *hx-commands* name
       (hash 'name name
             'description desc
             'usage ""
             'handler handler))]
    [(_ name #:description desc #:usage usage handler)
     (hash-set! *hx-commands* name
       (hash 'name name
             'description desc
             'usage usage
             'handler handler))]))

;; Run a registered command
(define (hx/run-command name args)
  (let ([cmd (hash-ref *hx-commands* name #f)])
    (if cmd
        (let ([handler (hash-ref cmd 'handler)])
          (handler args))
        (begin
          (hx/error (string-append "Unknown command: " name))
          1))))

;; Helper: check if a value is defined
(define-syntax defined?
  (syntax-rules ()
    [(_ name)
     (with-handler (lambda (e) #f)
       (begin name #t))]))

;; Helper: string contains check
(define (string-contains? str substr)
  (let loop ([i 0])
    (cond
      [(> (+ i (string-length substr)) (string-length str)) #f]
      [(string=? (substring str i (+ i (string-length substr))) substr) #t]
      [else (loop (+ i 1))])))

;; Helper: format string (simple implementation)
(define (format fmt . args)
  (let loop ([chars (string->list fmt)]
             [args args]
             [result '()])
    (cond
      [(null? chars) (list->string (reverse result))]
      [(and (char=? (car chars) #\~)
            (not (null? (cdr chars)))
            (char=? (cadr chars) #\a)
            (not (null? args)))
       (loop (cddr chars)
             (cdr args)
             (append (reverse (string->list (->string (car args)))) result))]
      [else (loop (cdr chars) args (cons (car chars) result))])))

;; Convert any value to string
(define (->string val)
  (cond
    [(string? val) val]
    [(number? val) (number->string val)]
    [(symbol? val) (symbol->string val)]
    [(boolean? val) (if val "#t" "#f")]
    [(list? val) (string-append "(" (string-join (map ->string val) " ") ")")]
    [else "<object>"]))

;; Join strings with separator
(define (string-join strs sep)
  (if (null? strs)
      ""
      (let loop ([strs (cdr strs)] [result (car strs)])
        (if (null? strs)
            result
            (loop (cdr strs) (string-append result sep (car strs)))))))
