; Parses all of standard input and writes out the datums to standard
; input, one per line.
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "parser.scm")

(define (parse-all-datums)
  (call-with-current-continuation error-setup)
  (let ((datum (read-datum)))
    (if (not (eof-object? datum))
        (begin (write datum)
               (newline)
               (parse-all-datums)
        )
    )
  )
)

(parse-all-datums)
