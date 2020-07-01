#lang racket

(require (for-syntax racket/list) xml)
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

(define-syntax (page/proto stx)
  ;; returns a webpage consisting of a name and an expression to be evaluated to return a page of some kind.
  ;; the bindings GET, POST and COOKIE are available to this expression, containing a hash of
  ;; those arguments, also REQ, containing the unprocessed request
  ;; syntax: (page-proto <path> <header-expr> <body-expr>)
  ;; header-expr should return a hash of strings to strings, body-expr should return a list of bytestrings
  (define path (second (syntax->datum stx)))
  (define header-expr (third (syntax->datum stx)))
  (define body-expr (fourth (syntax->datum stx)))
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
       (response/full 200 #"Okay" (current-seconds) #f
                      (hash-map ((λ (GET POST COOKIE REQ) ,header-expr) get-args post-args cookie req)
                                          (λ (k v) (header (string->bytes/utf-8 k) (string->bytes/utf-8 v))))
                      ((λ (GET POST COOKIE REQ) ,body-expr) get-args post-args cookie req))))))

(define-syntax (page stx)
  ;; page which expects an xexpr, no header expression
  (datum->syntax
   stx
   `(page/proto ,(second (syntax->datum stx)) (hash "Content-Type" "text/html; charset=utf-8")
                (list (string->bytes/utf-8 (xexpr->string ,(third (syntax->datum stx))))))))

(define-syntax (page/headers stx)
  ;; page which expects an xexpr
  (define dtm (syntax->datum stx))
  (datum->syntax
   stx
   `(page/proto ,(second (syntax->datum stx)) ,(let [[headers (third dtm)]]
                                                 (if (hash-has-key? headers "Content-Type")
                                                     headers
                                                     (hash-set headers "Content-Type" "text/html; charset=utf-8")))
                (list (string->bytes (xexpr->string ,(fourth (syntax->datum stx))))))))
 
;(define (stylesheet stx)
;  ;; page which expects a cssexpr
;  (datum->syntax
;   stx
;   `(page-proto ,(second (syntax->datum stx)) ,(third (syntax->datum stx)) response/cssexpr)))

(define-syntax (textpage stx)
  ;; page which expects a string, no header expression
  (datum->syntax
   stx
   `(page/proto ,(second (syntax->datum stx)) (hash "Content-Type" "text/plain; charset=utf-8")
                (string->bytes/utf-8 ,(third (syntax->datum stx))))))

(define-syntax (textpage/headers stx)
  ;; page which expects a string
  (define dtm (syntax->datum stx))
  (datum->syntax
   stx
   `(page/proto ,(second (syntax->datum stx)) ,(let [[headers (third dtm)]]
                                                 (if (hash-has-key? headers "Content-Type")
                                                     headers
                                                     (hash-set headers "Content-Type" "text/plain; charset=utf-8")))
                (string->bytes/utf-8 ,(fourth (syntax->datum stx))))))

(define-syntax (datapage stx)
  ;; page which expects a bytestring
  ;; (datapage <name> <mime-type[bytes]> <body-expr>)
  (datum->syntax
   stx
   `(page/proto ,(second (syntax->datum stx)) (hash "Content-Type" ,(third syntax->datum stx))
                ,(fourth (syntax->datum stx)))))

(define-syntax (datapage/headers stx)
  ;; page which expects a bytestring
  ;; (datapage/headers <name> <mime-type> <header-expr> <body-expr>)
  (define dtm (syntax->datum stx))
  (datum->syntax
   stx
   `(page/proto ,(second (syntax->datum stx)) ,(let [[headers (fourth dtm)]]
                                                 (if (hash-has-key? headers "Content-Type")
                                                     headers
                                                     (hash-set headers "Content-Type" (third (syntax->datum stx)))))
                (string->bytes/utf-8 ,(fifth (syntax->datum stx))))))

(define-syntax (page/full stx)
  (define dtm (syntax->datum stx))
  (datum->syntax
   stx
   `(page/proto ,(second dtm) ,(third dtm) ,(fourth dtm) #:code ,(fifth dtm) #:mime-type (sixth dtm) #:headers (seventh dtm))))

(define (response/bytes mime-type bs)
  ;; respond plain bytes with the given mime type
  (response/full 200 #"OK" (current-seconds) mime-type '() (list bs))) 

;(define (response/cssexpr cssx) 
;  (if (cssexpr? cssx)
;      (reponse/full 200 #"OK" (current-seconds) #"text/plain; charset=utf-8" '()
;                    (list (string->bytes/utf-8 (cssexpr->string cssx))))
;      (error (format "response/cssexpr: expected cssexpr, got ~a" cssx))))

(require web-server/servlet-dispatch)
(require web-server/servlet-env)
(provide page textpage datapage page/headers textpage/headers datapage/headers webapp (struct-out webpage)
         page/proto response/bytes
         (all-from-out web-server/servlet)
         (all-from-out web-server/dispatch)
         (all-from-out web-server/servlet-dispatch)
         (all-from-out web-server/servlet-env)
         (all-from-out xml))