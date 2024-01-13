(define-library (rsv)
  (import (scheme base)
          (scheme file)
          (scheme write)

          (only (guile) string-prefix? string-suffix?)

          (ice-9 ftw)
          (ice-9 match)
          (ice-9 pretty-print)

          (srfi srfi-1)
          (srfi srfi-64)

          (statprof))
  (export write-rsv
          read-rsv
          main)

  (begin
    (define ROW-TERMINATOR-BYTE 253)
    (define VALUE-TERMINATOR-BYTE 255)
    (define NULL-VALUE-BYTE 254)

    (define (write-rsv scm port)
      (match scm
        (() '())
        ((() rows ...)
         (write-u8 ROW-TERMINATOR-BYTE
                   port)
         (write-rsv rows
                    port))
        (((#f fields ...) rows ...)
         (write-u8 NULL-VALUE-BYTE
                   port)
         (write-u8 VALUE-TERMINATOR-BYTE
                   port)
         (write-rsv (cons fields rows)
                    port))
        (((field fields ...) rows ...)
         (write-bytevector (string->utf8 field)
                           port)
         (write-u8 VALUE-TERMINATOR-BYTE
                   port)
         (write-rsv (cons fields rows)
                    port))))

    (define (read-rsv port)
      (let loop ((rows '())
                 (current-row '())
                 (current-field #u8()))
        (match (read-u8 port)
          ;; End of file:
          ((? eof-object?)
           (reverse rows))

          ;; Value Terminator Byte:
          (#xff
           (cond
            ((eof-object? (peek-u8 port))
             (error "Premature end of file."))
            (current-field ;; Is not null, a string.
             (loop rows
                   (cons (utf8->string current-field)
                         current-row)
                   #u8()))
            (else ;; Is null.
             (loop rows
                   (cons #f
                         current-row)
                   #u8()))))

          ;; Null Value Byte:
          (#xfe
           (cond
            ((eof-object? (peek-u8 port))
             (error "Premature end of file."))
            ((= #xff (peek-u8 port))
             (loop rows
                   current-row
                   #f))
            (else
             (error "Expected a Value Terminator Byte, got:"
                    (peek-u8 port)))))

          ;; Row Terminator Byte:
          (#xfd
           (loop (cons (reverse current-row)
                       rows)
                 '()
                 #u8()))

          ;; Any byte:
          (some-byte
           (cond
            ((eof-object? (peek-u8 port))
             (error "Premature end of file."))
            ((= #xfe (peek-u8 port))
             (error "Unexpected Null Value Byte."))
            ((= #xfd (peek-u8 port))
             (error "Unexpected Row Terminator Byte."))
            (else
             (loop rows
                   current-row
                   (bytevector-append current-field
                                      (make-bytevector 1 some-byte)))))))))

    (define invalid-filenames
      (map (lambda (filename)
             (string-append "TestFiles/" filename))
           (filter (lambda (filename)
                     (and (string-prefix? "Invalid" filename)
                          (string-suffix? "rsv" filename)))
                   (scandir "TestFiles/"))))

    (define valid-filenames
      (map (lambda (filename)
             (string-append "TestFiles/" filename))
           (filter (lambda (filename)
                     (and (string-prefix? "Valid" filename)
                          (string-suffix? "rsv" filename)))
                   (scandir "TestFiles/"))))

      (define (get-bytevector-all port)
        (let ((output-bytevector-port (open-output-bytevector)))
          (let loop ()
            (let ((byte (read-u8 port)))
              (cond
               ((eof-object? byte)
                (get-output-bytevector output-bytevector-port))
               ((number? byte)
                (write-u8 byte output-bytevector-port)
                (loop))
               (else
                (error "Illegal value while reading file:" byte)))))))

      (define (run-back-and-forth-test original-rsv)
        (statprof
         (lambda ()
           (let* ((input-bytevector-port (open-input-bytevector original-rsv))
                  (scm (read-rsv input-bytevector-port))
                  (output-bytevector-port (open-output-bytevector)))
             (write-rsv scm output-bytevector-port)
             (test-equal original-rsv
                         (get-output-bytevector output-bytevector-port))))
         #:count-calls? #t))

      (define (run-back-and-forth-failing-test original-rsv)
        (statprof
         (lambda ()
           (let* ((input-bytevector-port (open-input-bytevector original-rsv)))
             (test-error (cons (read-rsv input-bytevector-port)
                               original-rsv))))
         #:count-calls? #t))

      (test-begin "RSV")

      (for-each (lambda (filename)
                  (write filename) (newline)
                  (call-with-input-file filename
                    (lambda (port)
                      (test-group filename
                                  (let ((original-rsv (get-bytevector-all port)))
                                    (display 'run-back-and-forth-failing-test) (newline)
                                    (run-back-and-forth-failing-test original-rsv))))))
                invalid-filenames)

      (for-each (lambda (filename)
                  (write filename) (newline)
                  (call-with-input-file filename
                    (lambda (port)
                      (test-group filename
                                  (let ((original-rsv (get-bytevector-all port)))
                                    (display 'run-back-and-forth-test) (newline)
                                    (run-back-and-forth-test original-rsv))))))
                valid-filenames)

      (test-end "RSV")))
