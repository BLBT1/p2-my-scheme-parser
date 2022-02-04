; Read-eval-print loop that uses read-datum for parsing input.
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "parser.scm")

; Infer the interpreter's representation of an unspecified value, so
; that the REPL does not print it.
(define void-object (if #f #f))

(define (void? obj)
  (eq? obj void-object)
)

; The actual read-eval-print loop:
; 1) Uses call/cc to invoke error-setup, passing the REPL's
;    continuation to the latter, which stores it for error handling.
; 2) Prints an interpeter prompt.
; 3) (Read)s an expression from standard input.
; 4) (Eval)uates the expression in the global environment.
; 5) (Print)s the result, if it is not unspecified.
; 6) Tail-recursively calls itself.
; The loop exists upon receiving an end-of-file.
(define (read-eval-print-loop env)
  (call-with-current-continuation error-setup)
  (display "scm> ")
  (flush-output)  ; non-standard plt-r5rs procedure
  (let ((datum (read-datum)))               ; read
    (if (not (eof-object? datum))
        (let ((result (eval datum env)))    ; eval
          (cond ((not (void? result))
                 (write result)             ; print
                 (newline)
                )
          )
          (read-eval-print-loop env)  ; move to next iteration
        )
        (newline)  ; exit, printing a newline on the way out
    )
  )
)

(read-eval-print-loop (interaction-environment))
