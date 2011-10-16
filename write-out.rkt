#lang racket

(provide (all-defined-out))

(define (write-out v)
  (with-output-to-file "commitment.log" #:exists 'append
    (lambda () (pretty-print v))))