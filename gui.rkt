#lang frtime

(require "guiml.rkt"
         frtime/gui/fred
         (only frtime/gui/simple value-e value-b)
         (all-except racket/gui/base send-event)
         (only racket/match match-define)
         (only racket/base for/list))

(provide (all-defined))
(define vert vertical-panel%)
(define --- horizontal-panel%)

(define H 67)

(define msg:commitment-box-goal (new-cell ""))
(define msg:commitment-box-status (new-cell ""))
(define msg:commitment-time (new-cell ""))
(define msg:commitment-name (new-cell ""))
(define msg:error (new-cell ""))
(define e:close (event-receiver))

(define e:set-stamp-time (new-cell never-e))
(define e:set-stamp-description (new-cell never-e))
(define e:set-commit-description (new-cell never-e))


(define top-ft-frame%
  (class ft-frame% (super-new) (define/augment (on-close) (inner #f on-close) (send-event e:close #t))))


(define-guiml
  (frame top-pane panel:commitment-box panel:commitment-done panel:stamp panel:commit-to
         but:break but:done but:success but:bounced but:stamp but:clear but:commit
         text:stamp-time text:stamp-description text:commit-description text:commit-time
         gui:msg:error)
  (top-ft-frame%
   frame (@ [label "Mindfulness"])
   (vert top-pane (@)
         (vert panel:commitment-box
               (@ (style '(border)) (border 20) (stretchable-height #f) (min-height H))
               (--- (@) (message% (@ (label "Commitment box:")))
                    (ft-message% (@ (label msg:commitment-box-goal)
                                    (min-width 200))))
               (--- (@)
                    (message% (@ (label "left:")))
                    (ft-message% (@ (label msg:commitment-box-status)
                                    (min-width 200)))
                    (ft-button% but:break (@ (label "Break it")))
                    (ft-button% but:done (@ (label "done") (stretchable-width #t)))))
         
         (vert panel:commitment-done
               (@ (style '(border)) (border 20) (stretchable-height #f) (min-height H))
               (--- (@)
                    (ft-message% (@ (label msg:commitment-time)
                                    (min-width 150)))
                    (message% (@ (label "commitment on"))))
               (--- (@)
                    (ft-message% (@ (label msg:commitment-name)
                                    (min-width 150)))
                    (message% (@ (label "done."))))
               (--- (@)
                    (ft-button% but:success (@ (label "success!")))
                    (ft-button% but:bounced (@ (label "bounced")))))
         
         (vert panel:stamp
               (@ (style '(border)) (border 20) (stretchable-height #f) (min-height H))
               (ft-text-field% text:stamp-time (@ (label "Time since last stamp:")
                                                  (init-value "")
                                                  (value-set e:set-stamp-time)))
               (--- (@) (ft-text-field% text:stamp-description
                                        (@ (label "spent on")
                                           (init-value "")
                                           (value-set e:set-stamp-description)))
                    (ft-button% but:stamp (@ (label "Ok")))
                    (ft-button% but:clear (@ (label "Clear")))))
         
         (vert panel:commit-to
               (@ (style '(border)) (border 20) (stretchable-height #f) (min-height H))
               (ft-text-field% text:commit-description (@ (label "Commit to")
                                                          (value-set e:set-commit-description)))
               (--- (@)
                    (ft-text-field% text:commit-time (@ (label "for")))
                    (message% (@ (label "minutes")))
                    (ft-button% but:commit (@ (label "Go")))))
         
         (ft-message% gui:msg:error (@ (min-width 300) (label msg:error))))))



(match-define (list e:break e:done e:success e:bounced e:stamp e:clear e:commit)
  (map value-e (list but:break but:done but:success but:bounced but:stamp but:clear but:commit)))

(match-define (list b:text:stamp-time b:text:stamp-description b:text:commit-description b:text:commit-time)
  (map value-b (list text:stamp-time text:stamp-description text:commit-description text:commit-time)))

(define b:stamp-time:has-focus (hold (send text:stamp-time get-focus-events) #f))
