#lang racket/base

(require racket/match
         racket/sequence

         graph
         threading)

(provide (struct-out system)

         make-system-scheduler
         run-system!
         run-systems!
         run-scheduler!)

;; system: Symbol (-> positive-integer? any)
(struct system (label procedure) #:transparent)

(define (make-system #:label a-symbol
                     #:procedure a-procedure)
  (system a-symbol a-procedure))

(define (run-system! sys d)
  ((system-procedure sys) d))

(define (run-systems! a-list d)
  (for ([a-system (in-list a-list)])
    (run-system! a-system d)))

;; system-scheduler:
;; systems : list of all systems
;; edges : list of pairs of system labels
;;
;; system-scheduler implements the minimum amount of gen:graph to support tsort
(struct system-scheduler (systems edges)
  #:methods gen:graph
  [(define (get-vertices sched)
     (define systems (system-scheduler-systems sched))
     (for/list ([labels (in-hash-keys systems)])
       labels))

   (define (in-neighbors sched a-label)
     (~>> (system-scheduler-edges sched)
          (sequence-map
           (lambda (edge)
             (match-define (list a b) edge)
             (cond
               [(eq? a a-label) b]
               [else #f])))
          (sequence-filter values)))])

(define (make-system-scheduler #:systems system-list
                               #:order   edge-list)
  (system-scheduler
   (for/hash ([sys (in-list system-list)])
     (values (system-label sys) sys))
   edge-list))

(define (run-scheduler! sched d)
  (define sorted-systems
    (for/list ([label (in-list (tsort sched))])
      (hash-ref (system-scheduler-systems sched) label)))
  (run-systems! sorted-systems d))
