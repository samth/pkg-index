#lang racket/base
(require racket/list
         net/url
         racket/port)

(define (upload! the-email the-password the-post [incremental? #f])
  (define the-url
    (url "https" #f "pkgd.racket-lang.org" #f #t
         (list (path/param "api" empty)
               (path/param "upload" empty))
         empty
         #f))
  (displayln the-email)
  #;
  (displayln the-post)
  
  (define (chunk v n)
    (define keys (hash-keys v))
    (cond [(< (length keys) n) (list v)]
          [else (define fst (take keys n))
                (define beginning
                  (for/hash ([(k val) v]
                             #:when (member k fst))
                    (values k val)))
                (define end
                  (for/hash ([(k val) v]
                             #:unless (member k fst))
                    (values k val)))
                (cons beginning
                      (chunk end n))]))
  
  (define the-posts (if incremental? (chunk the-post incremental?) (list the-post)))
  
  (displayln (length the-posts))
  (for/and ([the-post the-posts])
    (define bs
      (call/input-url the-url
                      (λ (url)
                        (post-pure-port the-url
                                        (with-output-to-bytes
                                         (λ ()
                                           (write (list the-email
                                                        (string->bytes/utf-8 the-password)
                                                        the-post))))))
                      (lambda (p) (begin0 (port->bytes p) (close-input-port p)))))
    (define v (port->string (open-input-bytes bs)))
    (displayln v)
    (sleep 10)
    v))

(module+ main
  (require racket/cmdline)
  (define inc #f)
  (command-line #:program "upload"
                #:once-each 
                ["--incremental" n "run" "run incrementally" (set! inc (string->number n))]
                #:args (email password)
                (if (upload! email password
                             (read (current-input-port))
                             inc)
                  (exit 0)
                  (exit 1))))
