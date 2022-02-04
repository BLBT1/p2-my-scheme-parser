; Tests reading specific token types. Input format is a token type
; on its own line, followed by the token itself on the next line,
; followed by the | character on its own line. Reads tokens until EOF
; is reached. Prints each read token to standard output.
; Example:
;   string
;   "hello world"
;   |
;   character
;   #\a
;   |
; Result:
;  (string "hello world")
;  (character #\a)
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "lexer.scm")

(define (test-token-types)
  (let ((type (read)))
    (cond ((equal? type 'boolean) (test-token read-boolean))
          ((equal? type 'character) (test-token read-character))
          ((equal? type 'identifier) (test-token read-identifier))
          ((equal? type 'number) (test-token read-number))
          ((equal? type 'punctuator) (test-token read-punctuator))
          ((equal? type 'string) (test-token read-string))
    )
  )
)

(define (test-token read-fn)
  (clear-line)
  (write (read-fn))
  (newline)
  (read-separator)
  (test-token-types)
)

(define (read-separator)
  (let ((chars (list (read-char) (read-char) (read-char)))
        (expected '(#\newline #\| #\newline)))
    (if (not (equal? chars expected))
        (begin (display "Expected | line [")
               (write (list->string expected))
               (display "] after token, got: ")
               (write (list->string chars))
               (newline)
        )
    )
  )
)

(test-token-types)
