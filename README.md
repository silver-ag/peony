# Peony Web Framework

A simple frontend on racket web-server. See the scribble documentation, but as a quick introduction:
```
(require peony)
(serve/dispatch
  (webapp
    (page index.html
      '(html (body (h1 "test webapp")
                   (a [[href "tests/get?param1=1&p2=hello"]] "go here"))))
    (page tests/get
      `(html (body (p ,(format "the get parameters are: ~a" GET)))))))
```
Where peony provides `webapp` and `page`, and passes through `serve/dispatch` from `web-server`.
