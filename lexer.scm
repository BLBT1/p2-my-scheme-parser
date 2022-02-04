; Lexer for Scheme. The following is the lexical specification that it
; handles:
;
; <token> --> <identifier> | <boolean> | <number>
;     | <character> | <string> | ( | ) | #( | ' | ` | , | ,@ | .
; <delimiter> --> <whitespace> | ( | ) | " | ;
; <whitespace> --> <space or newline>
; <comment> --> ; <all subsequent characters up to a line break>
; <atmosphere> --> <whitespace> | <comment>
; <intertoken space> --> <atmosphere>*
;
; <identifier> --> <initial> <subsequent>*
;     | <peculiar identifier>
; <initial> --> <letter> | <special initial>
; <letter> --> [a-z]
;
; <special initial> --> ! | $ | % | & | * | / | : | < | =
;     | > | ? | ^ | _ | ~
; <subsequent> --> <initial> | <digit> | <special subsequent>
; <digit> --> [0-9]
; <special subsequent> --> + | - | . | @
; <peculiar identifier> --> + | - | ...
;
; <boolean> --> #t | #f
; <character> --> #\ <any character> | #\ <character name>
; <character name> --> space | newline
;
; <string> --> " <string element>* "
; <string element> --> <any character other than " or \>
;     | \" | \\
;
; <number> --> <integer> | <decimal>
; <integer> --> <sign> <digit>+
; <decimal> --> <sign> <digit>+ . <digit>*
;     | <sign> . <digit>+
;
; <sign> --> <empty> | + | -
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "distribution.scm")

;;;;;;;;;;;;;;;;;;;

; Read a string token.
(define (read-string)
  (if (read-start #\" "not a string") ; string must start with "
      (read-string-tail '()) ; call helper function below
  )
)

; Read the rest of a string literal.
(define (read-string-tail read-so-far)
  (let ((next-char (get-non-eof-char))) ; read a single char
    (cond ((char=? next-char #\") ; end of string
           ; return a string token
           (token-make 'string (list->string (reverse read-so-far))))
          ((char=? next-char #\\) ; start of escape sequence
           ; read the rest of the escape sequence and recurse
           (read-string-tail (cons (read-escaped) read-so-far)))
          ; complete this procedure
    )
  )
)

; Read the rest of an escape sequence.
(define (read-escaped)
  (let ((escaped-char (get-non-eof-char)))
    (if (or (char=? escaped-char #\") (char=? escaped-char #\\))
        escaped-char
        (error "unrecognized escape sequence")
    )
  )
)

;;;;;;;;;;;;;;;;;;;

; Read a boolean token.
(define (read-boolean)
  (if (read-start #\# "not a boolean") ; boolean starts with #
      (read-boolean-tail)
  )
)

; Read the rest of a boolean literal.
(define (read-boolean-tail)
  (let (
    (next-char (get-non-eof-char)) 
    (cond 
      ((char=? next-char #\t) 
        (token-make 'boolean #t))
      (char=? next-char #\f)
        token-make 'boolean #f)
      (else (error "not a boolean"))
  ))
)

;;;;;;;;;;;;;;;;;;;

; Read a character token.
(define (read-character)
  (if (and (read-start #\# "not a character")  ; character must start
           (read-start #\\ "not a character")) ; with #\
      (read-character-tail)
  )
)

; Read the rest of a character literal.
;; how to handle # \newline ?
(define (read-character-tail)
 (let ((next-char (get-non-eof-char)))
    (cond
      ((char=? next-char #\s)
        (let ((next-char (get-non-eof-char)))
          (if (char=? next-char #\p)
            (let ((next-char (get-non-eof-char)))
              (if (char=? next-char #\a)
                (let ((next-char (get-non-eof-char)))
                  (if (char=? next-char #\c)
                    (let ((next-char (get-non-eof-char)))
                      (if (char=? next-char #\e)
                        (if (delimiter? (peek-char))
                          (token-make 'character #\space)
                          (error "not a character"))
                        (error "not a character")))
                  (error "not a character")))
              (error "not a character")))
            (error "not a character"))))
      ((not (delimiter? (peek-char)))
        (error "not a character"))
      (else (token-make 'character next-char))
    )
  )
)


;;;;;;;;;;;;;;;;;;;

; Determine if the given character is a sign character.
(define (sign? char)
  (or (char=? char #\+) (char=? char #\-))
)

; Determine if the given character is a digit.
(define (digit? char)
  (and (char>=? char #\0) (char<=? char #\9))
)

; Determine if the given character is a dot.
(define (dot? char)
  (char=? char #\.)
)

; Read a number token.
(define (read-number)
  (let ((next-char (get-non-eof-char)))
    (cond
      ((sign? next-char)
        (read-number-tail (string next-char))
      )
      ((digit? next-char)
        (read-integer-tail (string next-char))
      )
      ((dot? next-char)
        (read-decimal-tail (string next-char))
      )
      (else (error "not a number"))
    )
  )
)

; Read the rest of the number token.
(define (read-number-tail read-so-far)
  (let ((next-char (get-non-eof-char)))
    (cond
      ((digit? next-char)
        (read-integer-tail (string-append read-so-far (string next-char))))
      ((dot? next-char)
        (read-decimal-tail (string-append read-so-far (string next-char))))
      (else (error "not a number")))
  )
)

; Read the rest of the integer token.
(define (read-integer-tail read-so-far)
  (if (delimiter? (peek-char))
    (token-make 'number (string->number read-so-far))
    (let ((next-char (get-non-eof-char)))
      (cond
        ((dot? next-char) ;;; (string char) <- parse a char to a string
          (read-decimal-tail (string-append read-so-far (string next-char))))
        ((digit? next-char)
          (read-integer-tail (string-append read-so-far (string next-char))))
        (else (error "not a number"))))
  )
)


(define (read-decimal-tail read-so-far)
  (if (delimiter? (peek-char))
    (token 'number' (string->number read-so-far))
    (let 
        (cond 
          ((digit? next-char) 
            (read-decimal-tail (string-append read-so-far (srting next-char)))
          )
          (else (error "not a number"))
      )
    )
  )
)


;;;;;;;;;;;;;;;;;;;

; Determine if the given character is a special initial.
(define (special-initial? char)
  (or (char=? char #\!) (char=? char #\$) (char=? char #\%) (char=? char #\&) (char=? char #\*)
    (char=? char #\/) (char=? char #\:) (char=? char #\<) (char=? char #\=) (char=? char #\>)
    (char=? char #\?) (char=? char #\^) (char=? char #\_) (char=? char #\~))
)

; Determine if the given character is an initial.
(define (initial? char)
  (or (char-alphabetic? char) (special-initial? char))
)

; Determine if the given character is a special subsequent.
(define (special-subsequent? char)
  (or (char=? char #\+) (char=? char #\-) (char=? char #\.) (char=? char #\@))
)

; Determine if the given character is a subsequent.
(define (subsequent? char)
  (or (initial? char) (digit? char) (special-subsequent? char))
)

; Determine if the given char is a perculiar identifier.
(define (peculiar-identifier? char)
  (or (char=? char #\+) (char=? char #\-) (char=? char #\.))
)



; Read an identifier token.
(define (read-identifier)
  '() ; replace with your code
)


;;;;;;;;;;;;;;;;;;;


; Read a punctuator token (i.e. one of ( ) #( . ' ` , ,@ ).
(define (read-punctuator)
  '() ; replace with your code
)

;;;;;;;;;;;;;;;;;;;

; Read a comment. Discards the data and returns an unspecified value.
(define (read-comment)
  (if (read-start #\; "not a comment")
      (read-comment-tail)
  )
)

; Read the rest of a comment.
(define (read-comment-tail)
  (clear-line)
)


;;;;;;;;;;;;;;;;;;;

; Read a token, which can be a boolean, character, string, identifier,
; number, or punctuator. Discards whitespace and comments.
(define (read-token)
  (let ((next-char (peek-char)))
    (cond ((eof-object? next-char) ; eof
           (read-char)) ; just return eof
          ((whitespace? next-char) ; whitespace
           (read-char) ; discard it
           (read-token)) ; read another token
          ((char=? next-char #\;) ; comment
           (read-comment) ; discard it
           (read-token)) ; read another token
          ; complete this procedure
          (else
           (error "bad token"))
    )
  )
)
