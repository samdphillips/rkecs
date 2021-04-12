#lang racket/base

(require racket/sequence
         racket/set

         ecs/component
         ecs/private/bag)

(module+ test
  (require rackunit))

(provide make-entity-store
         entity-store?
         entity-store-all-components
         current-entity-store

         add-component!

         remove-all-with-tag!

         find-entities-with-tag
         find-entities-with-tags

         find-first-component-by-entity
         find-all-component-by-entity)

;; components-by-entity : Hash Entity       (Setof Component)
;; components-by-tag    : Hash ComponentTag (Setof Component)
;; entities-by-tag      : Hash ComponentTag (Bagof Entity)
(struct entity-store (components-by-entity components-by-tag entities-by-tag))

(define (make-entity-store)
  (entity-store (make-hasheq)
                (make-hasheq)
                (make-hasheq)))

(define current-entity-store (make-parameter #f))

(define (add-component! a-component
                        #:entity-store
                        [es (current-entity-store)])
  (define ((make-update! store-add store) get-table get-tag get-value)
    (hash-update! (get-table es)
                  (get-tag a-component)
                  (lambda (c*) (store-add c* (get-value a-component)))
                  store))
  (define update-set! (make-update! set-add seteq))
  (define update-bag! (make-update! bag-add empty-bag))
  (update-set! entity-store-components-by-entity
               component-entity
               values)
  (update-set! entity-store-components-by-tag
               component-tag
               values)
  (update-bag! entity-store-entities-by-tag
               component-tag
               component-entity))

(define (entity-store-num-components #:entity-store
                                     [es (current-entity-store)])
  (for/sum ([v (in-hash-values (entity-store-components-by-entity es))])
    (set-count v)))

(define (entity-store-all-components #:entity-store
                                     [es (current-entity-store)])
  (define components-by-tag
    (entity-store-components-by-tag es))
  (for/fold ([c (seteq)]) ([c* (in-hash-values components-by-tag)])
    (set-union c c*)))

#;#;#;
(define (remove-entity! an-entity
                        #:entity-store
                        [es (current-entity-store)])
  ...)

(define (remove-component/entity! a-component-or-tag
                                  an-entity
                                  #:entity-store
                                  [es (current-entity-store)])
  ...)

(define (remove-component! a-component
                           #:entity-store
                           [es (current-entity-store)])
  ...)


;; remove-all-with-tag! Tag [Store] -> Void
;; Removes all components with a specific tag
(define (remove-all-with-tag! a-tag
                              #:entity-store
                              [an-entity-store (current-entity-store)])
  (define entities
    (bag-unique-elements
     (hash-ref
      (entity-store-entities-by-tag an-entity-store) a-tag empty-bag)))
  (define components-by-entity
    (entity-store-components-by-entity an-entity-store))

  (hash-remove! (entity-store-components-by-tag an-entity-store) a-tag)
  (hash-remove! (entity-store-entities-by-tag an-entity-store) a-tag)
  (for ([entity (in-set entities)])
    (hash-update! components-by-entity
                  entity
                  (lambda (components)
                    (for/set ([a-component (in-set components)]
                              #:unless (eq? a-tag (component-tag a-component)))
                      a-component)))))


(module+ test
  (test-begin
   "remove components with tags"
   (define-component key name)
   (parameterize ([current-entity-store (make-entity-store)])
     (define k1 (make-key #:entity 'player #:name #\q))
     (define k2 (make-key #:entity 'player #:name #\w))
     (add-component! k1)
     (add-component! k2)
     (check-equal?
      (for/set ([c (find-all-component-by-entity 'player key@)]) c)
      (set k1 k2))
     (remove-all-with-tag! key@)
     (check-equal? (sequence-length
                    (find-all-component-by-entity 'player key@))
                   0))))


;; find-entities-with-tag : Tag [Store] -> (Setof Entity)
;; find all entities with a specific tag
(define (find-entities-with-tag component-tag
                                #:entity-store
                                [es (current-entity-store)])
  (bag-unique-elements
   (hash-ref (entity-store-entities-by-tag es)
             component-tag
             empty-bag)))


;; find-entities-with-tags : (Listof Tag) [Store] -> (Setof Entity)
;; find all entities with all of the specified tags
(define (find-entities-with-tags ctags
                                 #:entity-store
                                 [es (current-entity-store)])
  (define (empty? s)
    (and s (set-empty? s)))
  (define (intersect a b)
    (if a (set-intersect a b) b))
  (for/fold ([entities #f]) ([tag (in-list ctags)] #:break (empty? entities))
    (intersect entities (find-entities-with-tag tag))))


(module+ test
  (test-begin
   "find entities with tags"
   (define-component posn [x 0] [y 0])
   (define-component dir [dx 0] [dy 0])
   (parameterize ([current-entity-store (make-entity-store)])
     (add-component! (make-posn #:entity 'player))
     (add-component! (make-posn #:entity 'barrier #:x 10 #:y 10))
     (add-component! (make-dir  #:entity 'player))
     (check-equal? (set 'player 'barrier) (find-entities-with-tag posn@))
     (check-equal? (set 'player) (find-entities-with-tags (list dir@ posn@))))))


;; find-all-component-by-entity : Entity Tag [Store] -> (Sequenceof Component)
;; find all of the components on an entity with a specific Tag
(define (find-all-component-by-entity entity
                                      ctag
                                      #:entity-store
                                      [es (current-entity-store)])
  (sequence-filter
   (lambda (c)
     (eq? (component-tag c) ctag))
   (hash-ref (entity-store-components-by-entity es) entity null)))


(module+ test
  (test-begin
   "find all components"
   (define-component posn [x 0] [y 0])
   (parameterize ([current-entity-store (make-entity-store)])
     (define p1 (make-posn #:entity 'player))
     (add-component! p1)
     (define result1 (sequence->list
                      (find-all-component-by-entity 'player posn@)))
     (check-equal? result1 (list p1))

     (define p2 (make-posn #:entity 'player))
     (add-component! p2)
     (define result2
       (for/set ([component (find-all-component-by-entity 'player posn@)])
         component))
     (check-equal? (set p1 p2) result2))))


(define (find-first-component-by-entity an-entity
                                        a-component-tag
                                        #:entity-store
                                        [es (current-entity-store)])
  (for/first ([component
               (find-all-component-by-entity an-entity
                                             a-component-tag
                                             #:entity-store es)])
    component))


(module+ test
  (test-begin
   "find first components"
   (define-component posn [x 0] [y 0])
   (parameterize ([current-entity-store (make-entity-store)])
     (define result0 (find-first-component-by-entity 'player posn@))
     (check-equal? result0 #f)

     (define p1 (make-posn #:entity 'player))
     (add-component! p1)
     (define result1 (find-first-component-by-entity 'player posn@))
     (check-equal? result1 p1)

     (define p2 (make-posn #:entity 'player))
     (add-component! p2)
     (define result2 (find-first-component-by-entity 'player posn@))
     ;; no guarantee which will be returned
     (check-true (or (equal? result2 p2) (equal? result2 p1))))))



