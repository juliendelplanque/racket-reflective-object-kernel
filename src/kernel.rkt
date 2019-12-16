#lang racket

(require racket/dict)

;-- Primitive functions to manipulate objects. ---------------------------------
; Create an object of an arbitrary size.
(define (make-object size)
  (make-vector size null))

; Retrieve the data held by an object at a specific offset.
(define (object-ref object offset)
  (vector-ref object offset))

; Mutate the data held by an object at a specific offset.
(define (object-set! object offset value)
  (vector-set! object offset value))

;-------------------------------------------------------------------------------

;-- Primitive function to manipulate any object. -------------------------------

(define class-offset 0)

(define (get-object-class object)
  (object-ref object class-offset))

(define (set-object-class object new-class)
  (object-set! object class-offset new-class))

;-------------------------------------------------------------------------------

;-- Primitive functions to manipulate objects representing classes. ------------

(define name-offset 1)

(define (get-class-name class)
  (object-ref class name-offset))

(define (set-class-name class new-name)
  (object-set! class name-offset new-name))

(define superclass-name-offset 2)

(define (get-class-superclass-name class)
  (object-ref class superclass-name-offset))

(define (set-class-superclass-name class new-superclass)
  (object-set! class superclass-name-offset new-superclass))

(define instance-variables-offset 3)

(define (get-class-instance-variables class)
  (object-ref class instance-variables-offset))

(define (set-class-instance-variables class new-instance-variables)
  (object-set! class instance-variables-offset new-instance-variables))

(define keywords-offset 4)

(define (get-class-keywords class)
  (object-ref class keywords-offset))

(define (set-class-keywords class new-keywords)
  (object-set! class keywords-offset new-keywords))

(define methods-dictionary-offset 5)

(define (get-class-methods-dictionary class)
  (object-ref class methods-dictionary-offset))

(define (set-class-methods-dictionary class new-methods-dictionary)
  (object-set! class methods-dictionary-offset new-methods-dictionary))

(define (register-method class selector fct)
  (dict-set! (get-class-methods-dictionary class) selector fct))

(define (get-method class selector)
  (dict-ref (get-class-methods-dictionary class) selector))

(define (has-method class selector)
  (dict-has-key? (get-class-methods-dictionary class) selector))

;-------------------------------------------------------------------------------

;-- Primitive for instance creation --------------------------------------------

(define (allocate-new-instance class)
  (let ([instance (make-object (length (get-class-instance-variables class)))])
    (begin
      (set-object-class instance (get-class-name class))
      instance)))

;-------------------------------------------------------------------------------

;-- Primitives for classes management ------------------------------------------

(define (register-class namespace class-object)
  (dict-set! namespace (get-class-name class-object) class-object))

(define (get-class namespace symbol)
  (dict-ref namespace symbol))

;-------------------------------------------------------------------------------

;-- Macro magic for being able to refer to self easily -------------------------

; When we encounter self variable, we expand it as (bound-self).
; (bound-self) is a parameter that is bound to the object on which is called a
; method.
(define-syntax self
    (lambda (stx)
      (syntax-case stx ()
        [val (identifier? (syntax self)) (syntax (bound-self))])))

(define bound-self (make-parameter null))

;-------------------------------------------------------------------------------

;-- Primitives for sending messages --------------------------------------------

(define (lookup selector class namespace)
  (if (has-method class selector)
      (get-method class selector)
      (if (eq? (get-class-name class) 'Object)
          null
          (lookup selector (get-class namespace (get-class-superclass-name class))))))

; (define (send receiver selector arguments)
;   ;TODO
;   (apply (lookup selector (get-object-class receiver)) receiver arguments))

;-------------------------------------------------------------------------------

;-- Bootstraping the kernel ----------------------------------------------------

(define (build-namespace)
  (let
      ([namespace (make-hash)]
       [Class (make-object 6)])
    (begin
      (set-object-class Class 'Class)
      (set-class-name Class 'Class)
      (set-class-superclass-name Class 'Object)
      (set-class-instance-variables Class '(class name superclass iv keywords methodDict))
      (set-class-keywords Class '(name: superclass: iv: keywords: methodDict:))
      (set-class-methods-dictionary Class (make-hash))
      (register-class namespace Class)
      namespace)))

;-------------------------------------------------------------------------------

; Tests, to move in a separated file -------------------------------------------

; (require rackunit)
; 
; (define-test-suite grammar
;   (define foo x)
;   (test-case "optional"
;     (check-equal? (parse-result (optional (char #\a)) "a") #\a)
;     (check-equal? (parse-result (optional (char #\a)) "") null)
;   )
; )
; 
; (require rackunit/text-ui)
; (run-tests grammar)