#lang racket/base

(require racket/set)

(provide empty-bag
         bag-update
         bag-add
         bag-unique-elements)

(module+ test
  (require rackunit))

(struct bag (store))

(define empty-bag (bag (hasheq)))

(define (bag-update a-bag proc)
  (bag (proc (bag-store a-bag))))

(define (bag-add a-bag an-element)
  (bag-update a-bag (lambda (ht) (hash-update ht an-element add1 0))))

(define (bag-unique-elements a-bag)
  (for/set ([element (in-hash-keys (bag-store a-bag))]) element))

(module+ test
  (check-equal? (set) (bag-unique-elements empty-bag))
  (check-equal? (set 'a) (bag-unique-elements (bag-add empty-bag 'a)))
  (check-equal? (set 'a 'b) (bag-unique-elements
                             (bag-add (bag-add empty-bag 'a) 'b)))
  (check-equal? (set 'a 'b) (bag-unique-elements
                             (bag-add
                              (bag-add
                               (bag-add empty-bag 'a) 'b) 'a))))
