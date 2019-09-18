#lang racket/base

(require (for-syntax racket/base
                     racket/sequence
                     racket/syntax
                     syntax/parse
                     syntax/stx)
         syntax/parse/define)

(define entity? symbol?)

(struct component (entity) #:transparent)
(define-values (prop:component prop:component? prop:component-value)
  (make-struct-type-property 'prop:component))

(define (component-tag v)
  (prop:component-value v))

(begin-for-syntax
  (define-syntax-class component-field
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
      [field:component-field (and (attribute field.default) #t)]))
)

(define-syntax-parser define-component
  [(_ name:id fields:component-field ...)
   #:with
   (default-fields:component-field ...)
   (for/list ([stx (in-syntax #'(fields ...))]
              #:when (has-default? stx))
      stx)

   #:with
   (required-fields:component-field ...)
   (for/list ([stx (in-syntax #'(fields ...))]
              #:when (not (has-default? stx)))
      stx)

   #:with maker-name (format-id #'name "make-~a" #'name)

   #'(begin
       (struct name component ([fields.name #:mutable] ...)
         #:transparent
         #:property prop:component 'name)
       (define (maker-name entity
                           (~@ required-fields.keyword required-fields.name) ...
                           (~@ default-fields.keyword default-fields) ...)
         (name entity fields.name ...)))])


