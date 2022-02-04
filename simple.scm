; S --> aSb | C
; C --> cC | empty
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "distribution.scm")

; Parses the S nonterminal. Returns a list of two numbers, the first
; the number of a's and b's and the second the number of c's.
(define (parse-S)
  (let ((item (peek-char)))
    (cond ((char=? item #\a) ; next item is an a
           (read-char) ; remove the a from the input stream
           (let* ((recurse (parse-S)) ; recursively parse S
                  (next (read-char))) ; next item should be b
             (if (char=? next #\b)
                 ; add 1 to the a count, preserve c count
                 (cons (+ 1 (car recurse)) (cdr recurse))
                 (error "expected b")
             )
           ))
          (else (list 0 (parse-C))) ; 0 a's + however many c's
    )
  )
)

; Parses the C terminal. Returns the number of c's.
(define (parse-C)
  (let ((item (peek-char)))
    (cond ((char=? item #\c) ; next item is a c
           (read-char) ; remove the c from the input stream
           (+ 1 (parse-C))) ; add 1 to the recursive c count
          (else 0) ; no c's
    )
  )
)

(parse-S)
