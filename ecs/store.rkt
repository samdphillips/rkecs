#lang racket/base

(require racket/sequence
         racket/set
         racket/stream
         rebellion/collection/multiset
         "component.rkt")

(provide make-entity-store
         entity-store?         
         entity-store-all-components
         current-entity-store

         add-component!

         remove-all/tag!
         
         find-entities/tag
         find-entities/tags

         entity-component/first
         entity-component/all
         
         )

;; components-by-entity : Hash Entity (Setof Component)
;; components-by-tag : Hash ComponentTag (Setof Component)
;; entities-by-tag : Hash ComponentTag (Multisetof Entity)
(struct entity-store (components-by-entity components-by-tag entities-by-tag))

(define (make-entity-store)
  (entity-store (make-hasheq)
                (make-hasheq)
                (make-hasheq)))

(define (add-component! a-component
                        #:entity-store
                        [es (current-entity-store)])
  (define ((make-update! store-add store) get-table get-tag get-value)
    (hash-update! (get-table es)
                  (get-tag a-component)
                  (lambda (c*) (store-add c* (get-value a-component)))
                  store))
  (define update-set!      (make-update! set-add seteq))
  (define update-multiset! (make-update! multiset-add multiset))
  (update-set! entity-store-components-by-entity
               component-entity
               values)
  (update-set! entity-store-components-by-tag
               component-tag
               values)
  (update-multiset! entity-store-entities-by-tag
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

(define (remove-all/tag! a-tag
                         #:entity-store
                         [an-entity-store (current-entity-store)])
  (define entities
    (multiset-unique-elements
     (hash-ref
      (entity-store-entities-by-tag an-entity-store) a-tag empty-multiset)))
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

(define (find-entities/tag component-tag
                           #:entity-store
                           [es (current-entity-store)])  
  (multiset-unique-elements
   (hash-ref (entity-store-entities-by-tag es)
             component-tag
             empty-multiset)))

(define (find-entities/tags ctags
                            #:entity-store
                            [es (current-entity-store)])
  (define (empty? s)
    (and s (set-empty? s)))
  (define (intersect a b)
    (if a (set-intersect a b) b))
  (for/fold ([entities #f]) ([tag (in-list ctags)] #:break (empty? entities))
    (intersect entities (find-entities/tag tag))))



(define (entity-component/all entity
                              ctag
                              #:entity-store
                              [es (current-entity-store)])
  (sequence-filter
   (lambda (c)
     (eq? (component-tag c) ctag))
   (hash-ref (entity-store-components-by-entity es) entity null)))
  
(define (entity-component/first entity
                                ctag
                                #:entity-store
                                [es (current-entity-store)])
  (define components
    (sequence->stream
     (entity-component/all entity ctag #:entity-store es)))
  (and (not (stream-empty? components))
       (stream-first components)))

(define current-entity-store (make-parameter #f))

