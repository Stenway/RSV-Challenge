(define-library (rsv)
  (import (scheme base)
          (scheme file)
          (scheme write)
          (ice-9 match))
  (export scm->rsv
          rsv->scm
          main)

  (begin
    (define ROW-TERMINATOR-BYTE 253)
    (define VALUE-TERMINATOR-BYTE 255)
    (define NULL-VALUE-BYTE 254)

    (define (scm->rsv scm port)
      (match scm
        (()
         (write-u8 ROW-TERMINATOR-BYTE
                   port))
        ((() rows ...)
         (write-u8 ROW-TERMINATOR-BYTE
                   port)
         (scm->rsv rows
                   port))
        (((#f fields ...) rows ...)
         (write-u8 NULL-VALUE-BYTE
                   port)
         (write-u8 VALUE-TERMINATOR-BYTE
                   port)
         (scm->rsv (cons fields rows)
                   port))
        (((field fields ...) rows ...)
         (write-bytevector (string->utf8 field)
                           port)
         (write-u8 VALUE-TERMINATOR-BYTE
                   port)
         (scm->rsv (cons fields rows)
                   port))))

    (define (rsv->scm port)
      (let loop ((rows '())
                 (current-row '())
                 (current-field #u8()))
        (match (read-u8 port)
          ;; End of file:
          ((? eof-object?)
           (reverse rows))

          ;; Value Terminator Byte:
          (255
           (if current-field
               (loop rows
                     (cons (utf8->string current-field)
                           current-row)
                     #u8())
               (loop rows
                     (cons #f
                           current-row)
                     #u8())))

          ;; Null Value Byte:
          (254
           (loop rows
                 current-row
                 #f))

          ;; Row Terminator Byte:
          (253
           (loop (cons (reverse current-row)
                       rows)
                 '()
                 #u8()))

          ;; Any byte:
          (some-byte
           (loop rows
                 current-row
                 (bytevector-append current-field
                                    (make-bytevector 1 some-byte)))))))

    (define (main args)
      (let ((our-scm '(("blah" "blah" #f "moo")
                       ("asdf" "fdsaa" #f "asdfsd" "asdf")
                       ()
                       (#f #f))))
        (call-with-output-file "poop.rsv"
          (lambda (port)
            (scm->rsv our-scm port)))
        (call-with-input-file "poop.rsv"
          (lambda (port)
            (write (rsv->scm port))))))))
