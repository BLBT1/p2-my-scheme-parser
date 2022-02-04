; Parses data from standard input and checks that the result is
; equivalent to that from the built-in read function. Requires every
; datum to be repeated twice. The | character must appear after every
; datum pair.
; Example:
;   hello hello | +3.14 +3.14 |
;   "hello world" "hello world" | #t #t | #\a #\a |
;   'adsf 'adsf (define x 3) (define x 3)|
;   `(a b ,@ (c d , f '(g h)))
;   `(a b ,@ (c d , f '(g h)))|
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "test-util.scm")

(parse-all read-datum)
