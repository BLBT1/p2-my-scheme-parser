; Parser for Scheme. Parses the external representation of a Scheme
; datum:
;
; <datum> --> <simple datum> | <compound datum>
; <simple datum> --> <boolean> | <number> | <character>
;     | <string> | <symbol>
; <symbol> --> <identifier>
; <compound datum> --> <list> | <vector>
; <list> --> (<datum>*) | (<datum>+ . <datum>)
;     | <abbreviation>
; <abbreviation> --> <abbrev prefix> <datum>
; <abbrev prefix> --> ' | ` | , | ,@
; <vector> --> #(<datum>*)
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "lexer.scm")

;;;;;;;;;;;;;;;;;;;

; Determines whether the given token represents a simple datum.
(define (simple-datum? token)
  (let ((type (token-type token)))
    (memq type '(boolean number character string identifier))
  )
)

; Reads and parses a simple datum from standard input, returning the
; Scheme representation of the datum. Returns an eof object if eof is
; encountered. An error is raised if a non-simple datum is
; encountered.
(define (read-simple-datum)
  (let ((first-token (read-token)))
    (cond ((eof-object? first-token) first-token) ; just return eof
          ((not (simple-datum? first-token))
           (error "not a simple datum"))
          ; complete this procedure
    )
  )
)


;;;;;;;;;;;;;;;;;;;

; Reads and parses a full compound datum from standard input, which
; can be a proper list, a dotted list, a vector, or an abbreviation.
; Returns the Scheme representation of the datum. For an abbreviation,
; returns the corresponding list representation (e.g. 'hello -> (quote
; hello)). Returns an eof object if eof is encountered. An error is
; raised if a non-compound datum is encountered, or if the compound
; datum is improperly formatted.
(define (read-compound-datum)
  (let ((first-token (read-token)))
    (cond ((eof-object? first-token) first-token) ; just return eof
          ((simple-datum? first-token)
           (error "not a compound datum"))
          ; complete this procedure
    )
  )
)


;;;;;;;;;;;;;;;;;;;

; Reads a complete datum from standard input. If eof is encountered,
; returns an eof object. Raises an error if an improper token or datum
; is encountered.
(define (read-datum)
  (let ((first-token (read-token)))
    (if (eof-object? first-token)
        first-token ; just return eof
        (read-datum-helper first-token)
    )
  )
)

; Reads a datum from standard input, where the datum's first (and
; possibly only) token is first-token. Raises an error if the datum is
; not properly formatted.
(define (read-datum-helper first-token)
  '() ; replace with your code
)
