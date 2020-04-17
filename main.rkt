#lang racket

(require (for-syntax racket/list))
(require web-server/servlet web-server/dispatch)

(struct webpage
  (path body) #:transparent)

(define (webapp index . pages)
  ;; syntax: (webapp <page1> ... <pagen>)
  ;; a webapp is a dispatcher servlet, which can be run with (serve/servlet <webapp>) or similar
  (λ (req)
    (define name (string-join (map path/param-path (url-path (request-uri req))) "/"))
    ((webpage-body
      (if (equal? name "")
          index
          (foldl (λ (p c)
                   (if (equal? name (symbol->string (webpage-path p)))
                       p
                       c))
                 (page p404
                       '(html
                         (head (title "404"))
                         (body (center (h1 "404") (p "page not found")))))
                 (cons index pages))))
     req)))

(define-syntax (page stx)
  ;; returns a webpage consisting of a name and an expression to be evaluated to return an htmlexpr.
  ;; the bindings GET and POST are available to this expression, containing a hash of
  ;; those arguments, also REQ, containing the unprocessed request
  ;; syntax: (page <path> <expr>)
  (define path (second (syntax->datum stx)))
  (define expr (third (syntax->datum stx)))
  (datum->syntax
   stx
   `(webpage
     ',path
     (λ (req)
       (define get-args (foldl (λ (a h) (hash-set h (car a) (cdr a))) (hash) (url-query (request-uri req))))
       (define post-args (if (request-post-data/raw req)
                             (foldl (λ (a h)
                                      (if (equal? (count (curry equal? #\=) (string->list a)) 1)
                                          (let ([v (string-split a "=")])
                                            (hash-set h (first v) (second v)))
                                          h))
                                    (hash)
                                    (string-split (bytes->string/utf-8 (request-post-data/raw req)) "&"))
                             (hash)))
       (define cookie (let ([heads (filter (λ (h) (string-ci=?
                                                  (bytes->string/utf-8 (header-field h))
                                                  "cookie"))
                                          (request-headers/raw req))])
                        (if (empty? heads)
                            (hash)
                            (foldl (λ (a h)
                                      (if (equal? (count (curry equal? #\=) (string->list a)) 1)
                                          (let ([v (string-split a "=")])
                                            (hash-set h (string->symbol (first v)) (second v)))
                                          h))
                                    (hash)
                                    (string-split (bytes->string/utf-8 (header-value (first heads))) "; ")))))
       (response/xexpr ((λ (GET POST COOKIE REQ) ,expr) get-args post-args cookie req))))))

(require web-server/servlet-dispatch)
(require web-server/servlet-env)
(provide page webapp (struct-out webpage)
         (all-from-out web-server/servlet)
         (all-from-out web-server/dispatch)
         (all-from-out web-server/servlet-dispatch)
         (all-from-out web-server/servlet-env))