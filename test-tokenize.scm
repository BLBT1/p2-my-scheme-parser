; Tokenizes all of standard input and writes out the tokens to
; standard input, one per line.
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "lexer.scm")

(define (tokenize)
  (call-with-current-continuation error-setup)
  (let ((next-token (read-token)))
    (if (not (eof-object? next-token))
        (begin (write next-token)
               (newline)
               (tokenize)
        )
    )
  )
)

(tokenize)
