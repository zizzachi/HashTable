#lang racket/base

(require rackunit)
(require rackunit/text-ui)
;(require test-engine/racket-tests)


; Import our HashTable script
(require "HashTable.rkt")


(define tests-hash
  (test-suite
   "Tests of hash"
   (test-case
    "Empty string"
    (check-equal? (hash "") 0))
   (test-case
    "Same strings hash to the same value"
    (check-equal? (hash "A") (hash "A"))
    (check-equal? (hash "fizz buzz") (hash "fizz buzz"))
    (check-equal? (hash "!%^&*#") (hash "!%^&*#")))
   (test-case
    "Expected hash values"
    (check-equal? (hash "a") 97))
   (test-case
    "Different strings hash to different values"
    (check-not-equal? (hash "ab") (hash "ba")))))

(define tests-add!
  (test-suite
   "Tests of add!"
   (test-case
    "Can add multiple distinct keys"
    (let ([ht (hash-table-new)])
      (add! ht "the answer" 42)
      (add! ht "pair" (cons 'a 'b))
      (add! ht "meta" (hash-table-new))))
   (test-case
    "Cannot add an exisiting element"
    (let ([ht (hash-table-new)])
      (add! ht "a" 'val)
      (check-exn exn:fail? (lambda () (add! ht "a" 'lav)))))
   (test-case
    "Add enough elements to rehash the table"
    (let* ([ht (hash-table-new)]
           [vec-size (vector-ref ht 1)]
           )
   ))))


; Run tests
(run-tests tests-hash)
(run-tests tests-add!)

; QUESTIONS FOR SAM
; For testing add!, update!, etc. do we just want to test each function independently, or do we want to do integration tests with multiple functions
;   Also, should each test case have its own hash, or can we make one that is used by several test-cases