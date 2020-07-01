#lang scribble/manual

@(require (for-label racket
                     web-server
                     db
                     xml
                     "main.rkt"))

@title{Peony Web Framework}

@defmodule[peony]

Peony is a small frontend on @other-doc['(lib "web-server/scribblings/web-server.scrbl")]
which makes web development fit closer to the idiom familiar to people who've mainly
used php or similar in the past. With Peony, each webpage lives at a fixed path and
corresponds to a chunk of code that's executed to produce html when the page is accessed.
As long as that's all that's needed, the details of dispatching and such can be avoided.

Peony is under active development and the api should not be considered stable.

See also @other-doc['(lib "db/scribblings/db.scrbl")].

@section{Full Example}

As an example of use, say we want to create a webapp to demonstrate the use of GET and POST parameters.

First, we can set up a main page like so:
@racketblock[
(define index (page index.html
                    '(html (body (h1 "demo webapp")
                                 (a [[href "demos/get"]] "test get")
                                 (br)
                                 (a [[href "demos/post"]] "test post")))))]
Which defines @racket[index] as a @racket[webpage] with the path @racket[index.html] and a static body. Since we'll be using it as
the @racket[index] of our webapp it will be accessible at www.address.com/. we call it @racket[index.html]
just for the sake of tradition, since it has to have a non-empty name as well.


The GET page can be implemented using the GET binding like so:
@racketblock[
(define getpage (page demos/get
                      `(html (body (h1 "GET demo")
                                   (form [[method "get"]]
                                         (p "submit a GET request with paramenters:")
                                         (p "p1: " (input [[type "text"][name "p1"]]))
                                         (p "p2: " (input [[type "text"][name "p2"]]))
                                         (br)
                                         (button "submit"))
                                   (table
                                    (tr (th "Name") (th "Value"))
                                    ,@(hash-map GET (λ (key val) `(tr (td ,key) (td ,val)))))
                                   (a [[href "index.html"]] "back")))))]
(where ",@"@"" is @racket[unquote-splicing]). The resulting page is generated with new GET values each time it's loaded.

The POST page can be identical except for replacing each instance of 'get' with 'post' (noting that the one referencing the
actual binding must be all-caps @racket[POST]):
@racketblock[
(define postpage (page demos/post
                      `(html (body (h1 "POST demo")
                                   (form [[method "post"]]
                                         (p "submit a POST request with paramenters:")
                                         (p "p1: " (input [[type "text"][name "p1"]]))
                                         (p "p2: " (input [[type "text"][name "p2"]]))
                                         (br)
                                         (button "submit"))
                                   (table
                                    (tr (th "Name") (th "Value"))
                                    ,@(hash-map POST (λ (key val) `(tr (td ,key) (td ,val)))))
                                   (a [[href "index.html"]] "back")))))]


Then we can put it all together into a @racket[webapp] and serve it:
@racketblock[
(define app (webapp index getpage postpage))
(serve/servlet app #:servlet-regexp #rx"" #:servlet-path "")]
@racket[serve/servlet] with @racket[#:servlet-regexp #rx""] and @racket[#:servlet-path ""]
is equivalent to @racket[serve/dispatch], but doing it this way allows us to add other
@racket[serve/servlet]-specific parameters later (such as @racket[#:start-web-browser?]
and @racket[#:port]).

@section{Reference}

@defproc[(webapp [index webpage?] [pages webpage?] ...)
         (-> request? response?)]{
 Returns a servlet that can be passed to @other-doc['(lib "web-server/scribblings/web-server.scrbl")]'s
 @racket[serve/dispatch] (or similar, but the @racket[servlet-regexp]
 must be @racket[#rx""] in any case). The servlet serves each page at
 the url matching its @racket[path], and a fixed 404 page at all
 other paths. The first page, the @racket[index], is also served at the empty path.}

@defstruct[webpage ([path symbol?] [body (-> request? response?)])]{
 A structure for webpages, consisting of a path the page can be found at and a function
 mapping an http @racket[request] to a @racket[response]. The forms below generate slightly
 limited @racket[webpage]s automatically.}

@subsection{Defining Pages}

@defform[(page name contents)
         #:contracts ([name symbol?]
                      [contents xexpr?])]{
 Returns a @racket[webpage] whose path is @racket[name] and whose body is
 a @other-doc['(lib "web-server/scribblings/web-server.scrbl")] servlet - a
 function taking an http @racket[request] and returning a @racket[response].
 The response is a standard 200 response whose body consists of the html
 corresponding to the @racket[xexpr?] provided by @racket[contents]. The @racket[contents] expression
 has access to four values: @racket[GET], @racket[POST], @racket[COOKIE] and @racket[REQ],
 containing respectively a hash mapping GET arguments to their values, a hash mapping
 POST arguments to their values, a hash mapping cookies to their values, and the literal @racket[request] in full.
 In GET, POST and COOKIE, if there are multiple parameters with the same name then the earlier ones are
 shadowed by the later ones, the full query can be extracted by manually processing
 REQ if this is a problem.

 @racket[(page name contents)] is a shorthand for @racket[(page/proto name (hash "Content-Type" "text/html; charset=utf-8") (list (string->bytes/utf-8 (xexpr->string contents))))].}

@defform[(textpage name contents)
         #:contracts ([name symbol?]
                      [contents string?])]{
 Returns a @racket[webpage] whose path is @racket[name] and whose body is the string provided in the @racket[contents] expression.
 As with @racket[page], the expression for @racket[contents] has access to @racket[GET], @racket[POST], @racket[COOKIE] and @racket[REQ].

 @racket[(textpage name contents)] is a shorthand for a @racket[page/proto] expression.}

@defform[(datapage name mime-type contents)
         #:contracts ([name symbol?]
                      [mime-type bytes?]
                      [contents bytes?])]{
 Returns a @racket[webpage] whose path is @racket[name] and whose body is the bytestring data provided by the @racket[contents] expression, served with the
 specified @racket[mime-type]. Again, based on page/proto.}

@defform[(page/headers name headers contents)
         #:contracts ([name symbol?]
                      [headers hash?]
                      [contents xexpr?])]{
 Works like @racket[page], but with an additional @racket[headers] expression. The @racket[headers] expression has access to GET, POST, COOKIE and REQ like
 @racket[contents] does, and it should return a hash of strings to strings describing any HTTP headers that should be set in the response. These headers
 override any implicit headers (ie. the default Content-Type of 'text/html; charset=utf-8' and those mentioned in the documentation for @racket[response]).}

@defform[(textpage/headers name headers contents)
         #:contracts ([name symbol?]
                      [headers hash?]
                      [contents string?])]{
 To @racket[textpage] as @racket[page/headers] is to @racket[page].}

@defform[(datapage/headers name mime-type headers contents)
         #:contracts ([name symbol?]
                      [mime-type bytes?]
                      [headers hash?]
                      [contents bytes?])]{
 To @racket[datapage] as @racket[page/headers] is to @racket[page]. Note that any Content-Type specified in the headers will override the @racket[mime-type argument].}

@defform[(page/proto name headers contents)
         #:contracts ([name symbol?]
                      [headers hash?]
                      [contents (listof? bytes?)])]{
 This is a generic form to construct pages. It works like @racket[page/headers] and @racket[textpage/headers], but without making any assumptions about the nature
 and purpose of the contents or what Content-Type is appropriate (a list of bytestrings is the form that web-server servlets expect page contents to be in).
 Again, the @racket[headers] hash should take strings to strings.}