#lang frtime

(require
 "frtime-utils.rkt"
 "gui.rkt"
 (only racket/base for exit make-base-namespace)
 frtime/gui/fred
 (only frtime/core/frp exceptions)
 (only racket/match match)
 
 (only gmarceau/cut lambda-pipe pipe)
 (lifted racket/pretty pretty-print)
 (lifted racket/base open-input-string member number?)
 (lifted "write-out.rkt" write-out)
 )
(set-cell! raise-exceptions #f)
(remember (===> exceptions (lambda (v) (set-cell! msg:error (exn-message (first v))))))

(define (show-panes panes)
  (for ([c (send top-pane get-children)])
    (send top-pane delete-child c))
  
  (for ([p (append panes (list gui:msg:error))])
    (send top-pane add-child p)))

(define/lifted (set-mode! mode)
  (match mode
    ['idle (show-panes (list panel:commit-to panel:stamp))
           (send text:commit-description focus)]
    ['commit-box (show-panes (list panel:commitment-box))]
    ['commit-done (show-panes (list panel:commitment-done))
                  (send but:success focus)]))

(define e:commit-done (new-cell never-e))

(define (read-time str)
  (with-handlers ([void (lambda (e)
                          (set-cell! msg:error (hold (===> (timer-e 15) "") (exn-message e)))
                          #f)])
    (define minutes (eval (read (open-input-string str)) (make-base-namespace)))
    (if (number? minutes)
        (let ()
          (set-cell! msg:error "")
          (* 60 minutes))
        (error 'read "'~a\' is not a number" minutes))))

(define e:commit-valid (=#=> e:commit (// (read-time (value-now b:text:commit-time)))))
(define e:stamp-valid (=#=> e:stamp (// (read-time (value-now b:text:stamp-time)))))

(define b:current-mode (delay (hold (merge-e (===> e:success 'idle)
                                             (===> e:bounced 'idle)
                                             (===> e:commit-valid 'commit-box)
                                             (===> e:break 'idle)
                                             (===> e:done 'idle)
                                             (===> e:commit-done 'commit-done))
                                    'idle)))


(define b:commit-time-left (new-cell 0))

(remember
 (===> e:commit-valid
       (// (pipe (read-time (value-now b:text:commit-time))
                 (seconds-countdown-b <>)
                 (set-cell! b:commit-time-left <>)))))

(set-cell! e:commit-done (when-e (and (eq? b:current-mode 'commit-box)
                                      (<= b:commit-time-left 0))))

(remember (set-mode! b:current-mode))

;--

(define (the-commit-desc)
  (snapshot (b:text:commit-description b:text:commit-time)
            (list b:text:commit-description
                  (read-time b:text:commit-time))))


(define e:any-button-except-commitment-report
  (merge-e
   (===> e:stamp-valid (// (snapshot (b:text:stamp-description b:text:stamp-time)
                                     (list 'stamp b:text:stamp-description (read-time b:text:stamp-time)))))
   (===> e:commit-valid (// (list* 'commit (the-commit-desc))))
   (===> e:break (// (list* 'break (the-commit-desc))))
   (===> e:done (// (list* 'done (the-commit-desc))))
   (===> e:clear (list 'clear))
   (===> e:commit-done (// (list* 'commit-done (the-commit-desc))))))

(define e:any-button (merge-e e:any-button-except-commitment-report
                              (===> e:success (// (list* 'success (the-commit-desc))))
                              (===> e:bounced (// (list* 'bounced (the-commit-desc))))))

(define b:last-time-stamp (hold (===> e:any-button-except-commitment-report seconds) (value-now seconds)))
(define b:time-since-last-stamp (- seconds b:last-time-stamp))

(define (to-minute-label round-fn v)
  (format "~a minutes" (round-fn (/ v 60))))

(define b:desired-stamp-time (to-minute-label floor b:time-since-last-stamp))
(send text:stamp-time set-value (value-now b:desired-stamp-time))

(set-cell! e:set-stamp-time (merge-e (=#=> (changes b:desired-stamp-time) (not b:stamp-time:has-focus))
                                     (===> e:stamp-valid b:desired-stamp-time)))

(set-cell! e:set-stamp-description (merge-e (===> e:stamp-valid (// (send text:stamp-description focus) ""))
                                            (===> e:success b:text:commit-description)))


(define (write-event data)
  (write-out (cons (seconds->date (value-now seconds)) data)))

(write-event '(0 boot))

(remember (===> e:any-button
                (lambda-pipe (cons (value-now b:time-since-last-stamp) <>)
                             (write-event <>))))

(set-cell! msg:commitment-box-goal b:text:commit-description)
(set-cell! msg:commitment-box-status (to-minute-label ceiling b:commit-time-left))
(set-cell! msg:commitment-time (hold (===> e:commit-valid (to-minute-label ceiling b:commit-time-left)) ""))
(set-cell! msg:commitment-name b:text:commit-description)

(remember (===> e:close (// (write-event (list (value-now b:time-since-last-stamp) 'quit)) (exit))))

(send frame show #t)
