#lang frtime

(require (only srfi/26 cut)
         (as-is racket/list take)
         (as-is:unchecked racket/base make-weak-hasheq hash-set! hash-remove!)
         (only frtime/core/frp snap?))

(provide ===>
         =#=>
         //
         seconds-countdown-b
         timer-e
         **
         remember
         history-e
         history-b
         delay
         delay-by-e
         event-loop-e
         event-loop-b
         event-loop-e/many
         event-loop-b/many
         define/lifted)

;; on event, send a event with the value of either:
;; -- the given value
;; -- a snapshot the given behaviors
;; -- the result of calling the given thunk
;; -- the result of calling the given 1-ary function to the event's value
;; -- the result of applying the given n-ary funciton to the event's value (assuming the event value is a list)
(define (===> e fn/v)
  (cond [(not (procedure? fn/v)) (==> e (lambda (_) (deep-value-now fn/v)))]
        [(procedure-arity-includes? fn/v 0) (==> e (lambda (_) (fn/v)))]
        [(procedure-arity-includes? fn/v 1) (==> e fn/v)]
        [else (==> e (lambda (v) (apply fn/v v)))]))

;; on event, keep the event if either:
;; -- the given value if true
;; -- a snapshot the given behaviors is true
;; -- the result of calling the given thunk is true
;; -- the result of calling he given 1-ary function to the event's value is true
;; -- the result of applying the given n-ary funciton to the event's value (assuming the event value is a list) is true
(define (=#=> e fn/v)
  (cond [(not (procedure? fn/v)) (=#> e (lambda (_) (deep-value-now fn/v)))]
        [(procedure-arity-includes? fn/v 0) (=#> e (lambda (_) (fn/v)))]
        [(procedure-arity-includes? fn/v 1) (=#> e fn/v)]
        [else (=#> e (lambda (v) (apply fn/v v)))]))



;; create a thunk, unless
;; the only body contain <>, in which case it creates a function with as many
;; arguments as <>'s
(define-syntax // (syntax-rules ()
                    [(_ (body ...)) (cut body ...)]
                    [(_ body ...) (lambda () body ...)]))

(define-syntax //snap (syntax-rules ()
                        [(_ (body ...)) (lambda args (parameterize ([snap? #t]) (apply (cut body ...) args)))]
                        [(_ body ...) (lambda args (parameterize ([snap? #t]) body ...))]))

(define (seconds-countdown-b sec)
  (- (+ (value-now seconds) sec) seconds))

;; create a event which trigger #t in SEC seconds
(define (timer-e sec)
  (define result (==> (when-e (<= (seconds-countdown-b sec) 0))
                      (lambda (v)
                        (hash-remove! remembered-hash result))))
  (hash-set! remembered-hash result result)
  result)

(define ** lift-strict)

;; achor top-levels signals so that they are not garbage collected
(define-syntax (remember stx)
  (syntax-case stx ()
    [(_ expr) (syntax/loc stx (define remembered expr))]))

(define (history-e e len)
  (event-loop-e ([state empty])
                [e => (lambda (ei) (take (cons ei state) (min len (add1 (length state)))))]))

(define (history-b b len)
  (hold (history-e (changes b) len) empty))

(define (delay b) (delay-by b 0))

(define remembered-weakhash (make-weak-hasheq))
(define remembered-hash (make-weak-hasheq))
(define (delay-by-e e s)
  (define result (event-receiver))
  (define event (==> e (lambda (v) (===> (timer-e s) (// (send-event result v))))))
  (hash-set! remembered-weakhash e event)
  result)

(define-syntax (define/lifted stx)
  (syntax-case stx ()
    [(_ (name arg ...) body ...)
     (syntax/loc stx (define name (lambda (arg ...) (lift-strict (lambda (arg ...) body ...) arg ...))))]))

(define-syntax (event-loop-help stx)
  (syntax-case stx ()
    [(_ ([name expr] ...)
        [e => body] ...)
     (with-syntax ([args #'(name ...)])
       #'(accum-e
          (merge-e
           (e . ==> . (lambda (v)
                        (lambda (state)
                          (call-with-values
                           (lambda ()
                             (apply
                              (lambda args (body v))
                              state))
                           list)))) ...)
          (list expr ...)))]))

(define-syntax (event-loop-e/many stx)
  (define (add-arrow clause)
    (syntax-case clause (=>)
      [(e => body) #'(e => body)]
      [(e body) #'(e => (lambda (_) body))]))
  
  (syntax-case stx ()
    [(_ ([name expr] ...)
        clause ...)
     (with-syntax ([(new-clause ...)
                    (map add-arrow (syntax->list #'(clause ...)))])
       #'(event-loop-help
          ([name expr] ...)
          new-clause ...))]))

(define-syntax (event-loop-b/many stx)
  (syntax-case stx ()
    [(_ ([name expr] ...)
        clause ...)
     #'(let ([name expr] ...)
         (hold (event-loop-e/many ([name name] ...) clause ...)
               (list name ...)))]))

(define-syntax (event-loop-e stx)
  (syntax-case stx ()
    [(_ body ...) (syntax/loc stx (==> (event-loop-e/many body ...) first))]))

(define-syntax (event-loop-b stx)
  (syntax-case stx ()
    [(_ body ...) (syntax/loc stx (first (event-loop-b/many body ...)))]))


