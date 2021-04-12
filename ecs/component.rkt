#lang racket/base

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax)
         racket/struct
         syntax/parse/define
         rebellion/type/struct)

(provide (rename-out [prop:component-value component-tag])
         component?
         component-entity
         define-component)

(begin-for-syntax
  (define-syntax-class default-field
    #:attributes (name default keyword)
    [pattern name:id
             #:attr default #f
             #:attr keyword (id->syntax-keyword #'name)]
    [pattern (name:id default:expr)
             #:attr keyword (id->syntax-keyword #'name)])

  (define (id->syntax-keyword stx)
    (datum->syntax
     stx
     (string->keyword
      (symbol->string
       (syntax->datum stx)))))

  (define (has-default? stx)
    (syntax-parse stx
      [field:default-field (and (attribute field.default) #t)]))

  (define (collect-constructor-args args modf)
    (for/list ([stx (in-syntax args)]
               #:when (modf (has-default? stx)))
      stx)))

(define (struct-descriptor-updater struct-descr)
  (let ([accessor (struct-descriptor-accessor struct-descr)]
        [mutator (struct-descriptor-mutator struct-descr)])
    (lambda (s i f)
      (mutator s i (f (accessor s i))))))

(struct component (entity))
(struct component-tag (name)
  #:constructor-name
  make-component-tag

  #:property
  prop:custom-write
  (make-constructor-style-printer
   (lambda (v) 'component-tag)
   (lambda (v) (list (component-tag-name v)))))

(define-values (prop:component prop:component? prop:component-value)
  (make-struct-type-property 'prop:component))

(define (default-component-printer descr fields)
  (make-constructor-style-printer
   (lambda (v) (struct-descriptor-name descr))
   (lambda (v)
     (let ([accessor (struct-descriptor-accessor descr)])
       (list* (unquoted-printing-string "#:entity")
              (component-entity v)
              (for/fold ([fv* null] #:result (reverse fv*))
                        ([i (in-naturals)]
                         [f (in-list fields)])
                (list* (accessor v i)
                       (unquoted-printing-string (format "#:~a" f))
                       fv*)))))))

(define-syntax-parse-rule (define-component name:id fields:default-field ...)
  #:with tag-name
  (format-id #'name #:subs? #t "~a@" #'name)
  #:with pred-name
  (format-id #'name #:subs? #t "~a?" #'name)
  #:with constructor-name
  (format-id #'name #:subs? #t "make-~a" #'name)
  #:with num-fields
  (length (syntax->list #'(fields ...)))

  (begin
    (define tag-name (make-component-tag 'tag-name))
    (define name
      (make-struct-implementation
       #:name 'name
       #:super-type struct:component
       #:mutable-fields 'num-fields
       #:constructor-name 'constructor-name
       #:property-maker
       (lambda (descr)
         (list (cons prop:component tag-name)
               (cons prop:custom-write
                     (default-component-printer descr '(fields.name ...)))))))

    (define pred-name
      (struct-descriptor-predicate name))

    (define-component-constructor name fields ...)
    (define-component-accessors name fields.name ...)))

(define-syntax-parse-rule (define-component-constructor struct-name:id
                            fields:default-field ...)
  #:with
  constructor-name
  (format-id #:subs? #t #'struct-name "make-~a" #'struct-name)

  #:with
  (required-args:default-field ...)
  (collect-constructor-args #'(fields ...) not)

  #:with
  (default-args:default-field ...)
  (collect-constructor-args #'(fields ...) values)

  (begin
    (define maker (struct-descriptor-constructor struct-name))
    (define (constructor-name #:entity entity
                              (~@ required-args.keyword required-args.name) ...
                              (~@ default-args.keyword default-args) ...)
      (maker entity fields.name ...))))

(define-syntax-parse-rule (define-component-accessors struct-name:id fields:id ...)
  #:with
  ([field-indices field-mutators field-accessors field-updaters] ...)
  (for/list ([field (in-syntax #'(fields ...))]
             [i (in-naturals)])
    (list #`'#,i
          (format-id field
                     #:subs? #t
                     "set-~a-~a!" #'struct-name field)
          (format-id field
                     #:subs? #t
                     "~a-~a" #'struct-name field)
          (format-id field
                     #:subs? #t
                     "update-~a-~a!" #'struct-name field)))

  (begin
    (define accessor (struct-descriptor-accessor struct-name))
    (define mutator  (struct-descriptor-mutator  struct-name))
    (define updater  (struct-descriptor-updater  struct-name))

    (define-values (field-mutators ...)
      (values (procedure-rename
               (lambda (s v)
                 (mutator s field-indices v))
               'field-mutators)
              ...))

    (define-values (field-accessors ...)
      (values (procedure-rename
               (lambda (s)
                 (accessor s field-indices))
               'field-accessors)
              ...))

    (define-values (field-updaters ...)
      (values (procedure-rename
               (lambda (s f)
                 (updater s field-indices f))
               'field-updaters)
              ...))))

