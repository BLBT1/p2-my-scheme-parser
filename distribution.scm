; Distribution code for Scheme lexer and parser.
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

;;;;;;;;;;;;;;;;;;
; Error handling ;
;;;;;;;;;;;;;;;;;;

; Remove all characters from standard input up to and including the
; next newline.
(define (clear-line)
  (let ((next-char (read-char)))
    (if (not (or (eof-object? next-char)
                 (char=? next-char #\newline)
             ))
        (clear-line)
    )
  )
)

; We store the REPL's continuation so that it can be invoked from the
; error procedure.
(define repl-continuation '())

(define (error-setup cont)
  (set! repl-continuation cont)
)

; Infer the interpreter's representation of an unspecified value, so
; that the REPL does not print it.
(define void-object (if #f #f))

; Prints an error message and writes a representation of the remaining
; arguments, if any. Discards the remainder of the input line and
; invokes the REPL's continuation.
(define (error message . args)
  (display "Error: ")
  (display message)
  (cond ((not (null? args))
         (display " ")
         (write (if (= (length args) 1)
                    (car args)
                    args
                )
         ))
  )
  (newline)
  (clear-line)
  (repl-continuation void-object)
)

; Set default continuation.
(call-with-current-continuation error-setup)


;;;;;;;;;;;;;
; Utilities ;
;;;;;;;;;;;;;

; Read a single non-eof character from standard input. Aborts and
; prints out an error message if eof is read.
(define (get-non-eof-char)
  (let ((char (read-char)))
    (if (eof-object? char)
        (error "end of file")
        char
    )
  )
)

; Determines if the given character is whitespace (i.e. a space or
; newline).
(define (whitespace? char)
  (or (char=? char #\space) (char=? char #\newline))
)

; Determines if the given character is a delimiter (i.e. whitespace,
; eof, parentheses, double quote, or semicolon).
(define (delimiter? char)
  (or (eof-object? char)
      (whitespace? char)
      (char=? char #\() (char=? char #\))
      (char=? char #\") (char=? char #\;)
  )
)

; Reads a single character from standard input. If the character read
; does not match expect, then lexing is aborted and error-message
; printed out.
(define (read-start expect error-message)
  (let ((first-char (get-non-eof-char)))
    (if (char=? first-char expect)
        #t
        (error error-message)
    )
  )
)


;;;;;;;;;;;;;
; Token ADT ;
;;;;;;;;;;;;;

; Constructs a token of the given type with the given value.
(define (token-make type value)
  (list type value)
)

; Retrieves the type of the given token.
(define (token-type token)
  (car token)
)

; Retrieves the data value of the given token.
(define (token-data token)
  (cadr token)
)
