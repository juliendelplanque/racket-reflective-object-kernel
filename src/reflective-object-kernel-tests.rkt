#lang racket

(require "reflective-object-kernel.rkt")
(require rackunit)

(define-test-suite object-kernel
  (test-case "test-make-object"
    (check-equal? (make-object 5) (vector null null null null null))
  )
  (test-case "test-object-size"
    (check-equal? (object-size (make-object 5)) 5)
  )
  (test-case "test-object-ref-set!"
    (let ([o (make-object 3)])
      (check-equal? (object-ref o 0) null)
      (check-equal? (object-ref o 1) null)
      (check-equal? (object-ref o 2) null)
      (object-set! o 1 42)
      (check-equal? (object-ref o 0) null)
      (check-equal? (object-ref o 1) 42)
      (check-equal? (object-ref o 2) null))
  )

  (test-case "test-get-set-object-class-name"
    (let ([o (make-object 2)])
      (check-equal? (get-object-class-name o) null)
      (set-object-class-name o 'Object)
      (check-equal? (get-object-class-name o) 'Object)))

  (test-case "test-get-set-class-name"
    (let ([class (make-class)])
      (check-equal? (get-class-name class) null)
      (set-class-name class 'Point)
      (check-equal? (get-class-name class) 'Point)))

  (test-case "test-get-set-class-superclass-name"
    (let ([class (make-class)])
      (check-equal? (get-class-superclass-name class) null)
      (set-class-superclass-name class 'Object)
      (check-equal? (get-class-superclass-name class) 'Object)))

  (test-case "test-get-set-class-instance-variables"
    (let ([class (make-class)])
      (check-equal? (get-class-instance-variables class) null)
      (set-class-instance-variables class '(x y))
      (check-equal? (get-class-instance-variables class) '(x y))))
  
  (test-case "test-get-set-class-methods-dictionary"
    (let (
          [class (make-class)]
          [methods-dict (make-methods-dictionary)])
      (check-equal? (get-class-methods-dictionary class) null)
      (set-class-methods-dictionary class methods-dict)
      (check-equal? (get-class-methods-dictionary class) methods-dict)))

  (test-case "test-register-method-has-method-get-method"
    (let (
          [class (make-class)]
          [methods-dict (make-methods-dictionary)]
          [method-lambda (lambda () 42)])
      (set-class-methods-dictionary class methods-dict)
      (check-false (has-method class 'foo))
      (register-method class 'foo method-lambda)
      (check-equal? (get-method class 'foo) method-lambda)))

  (test-case "test-allocate-new-instance"
    (let ([hand-made-class (make-class)])
      (set-class-instance-variables hand-made-class '(class x y))
      (check-equal? (object-size (allocate-new-instance hand-made-class)) 3)))

  (test-case "test-compute-extended-instance-variables"
    (let ([hand-made-class (make-class)])
      (set-class-instance-variables hand-made-class '(class x y))
      (check-equal? (compute-extended-instance-variables hand-made-class '(z)) '(class x y z))))
)

(require rackunit/text-ui)
(run-tests object-kernel)