#lang racket/base

(require rebellion/type/record)

(provide system
         system?
         system-name
         system-procedure

         run-system!
         run-system*!)

;; system: label (-> positive-integer? any)
(define-record-type system (name procedure))

(define (run-system! sys d)
  ((system-procedure sys) d))

(define (run-system*! sys* d)
  (for ([sys (in-list sys*)])
    (run-system! sys d)))

;; system-manager: directed graph of systems