#lang scheme/gui

(require mzlib/match)

(define-struct widget (object id semaphore))
(provide widget widget-object widget-id widget-semaphore guiml/parent)

(define-syntax (guiml/parent stx)
  (syntax-case stx ()
    [(_ parent item)
     (syntax/loc stx (first (guiml-child (parent item))))]))

(define-syntax guiml-child
  (syntax-rules (@)
    ((_ (parent-binding)) null)
    
    ((_ (parent-binding (head id (@ . properties) . tl) . siblings))
     (cons
      (guiml (head id (@ (parent parent-binding) . properties)
                   . tl))
      (guiml-child (parent-binding . siblings))))
    
    ((_ (parent-binding (head (@ . properties) . tl) . siblings))
     (cons
      (guiml (head (@ (parent parent-binding) . properties) . tl))
      (guiml-child (parent-binding . siblings))))
    ((_ (parent-binding (head id . tl) . siblings))
     (guiml-child (parent-binding (head id (@) . tl) . siblings)))
    ((_ (parent-binding (head . tl) . siblings))
     (guiml-child (parent-binding (head (@) . tl) . siblings)))))

(define-syntax guiml
  (syntax-rules (@)
    ((_ (name id (@ . properties)))
     (make-widget (new name . properties) id (make-semaphore 1)))
    
    ((_ (name id (@ . properties) first-child . rest-children))
     (let ((top (new name . properties)))
       (cons (make-widget top id (make-semaphore 1))
             (guiml-child (top first-child . rest-children)))))
    
    ;; The ID field is optional and defaults to #f.
    ((_ (name (@ . properties)))
     (guiml (name #f (@ . properties))))
    
    ;; The properties field is optional for a widget that
    ;; has no children, if an ID is specified.
    
    ((_ (name id)) (make-widget (new name) id (make-semaphore 1)))
    
    ((_ (name . rest))
     (guiml (name #f . rest)))))

(define-syntax sendmsg
  (syntax-rules ()
    ((_ widget args ...)
     (send (get-widget widget) args ...))))
(provide sendmsg)

(define (recursive-find pred list-data)
  (match list-data
    (() #f)
    (((? pred hd) . tl) hd)
    (((? pair? hd) . tl)
     (or (recursive-find pred hd)
	 (recursive-find pred tl)))
    ((_ . tl) (recursive-find pred tl))
    (x #f)))

(define (get-widget w)
  (if (object? w)
      w
      (widget-object (if (pair? w)
                         (car w) w))))

(define (get-widget/id top-widget id (compare eq?))
  (recursive-find
   (lambda (widget)
     (and (widget? widget)
          (compare (widget-id widget) id)))
   top-widget))

(provide get-widget/id)

;;  (define (guiml form)
;;   (construct-widget (guiml-internal form)))
(provide guiml-child)
(provide guiml)

(provide define-guiml)
(define-syntax (define-guiml stx)
  (syntax-case stx ()
    [(_ (name ...) item ...)
     (syntax/loc stx
       (define-values
         (name ...)
         (let* ([name (gensym)] ...
                [main (guiml item ...)])
           (values (widget-object (get-widget/id main name)) ...))))]))






