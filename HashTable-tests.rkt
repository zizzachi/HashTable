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
      (add! ht "pair" (cons 'a 'b))
      (add! ht 
      "
  __
// o\\
\\_ //
  <||
  <||
  <||"
      'literal)
      (add! ht "meta" (hash-table-new))
      ; Check number of pairs has increased
      (check-equal? (vector-ref ht 0) 3)))
   (test-case
    "Cannot add an exisiting element"
    (let ([ht (hash-table-new)])
      (add! ht "a" 'val)
      (check-exn exn:fail? (lambda () (add! ht "a" 'lav)))))
   (test-case
    "Add enough elements to rehash the table"
    (let* ([ht (hash-table-new)]
           [vec-size (vector-ref ht 1)]
           [rehash-size (+ (* PERCENT-FULL vec-size) 1)])
      (let kernel ([index 0])
        (unless (> index rehash-size)
          (add! ht (number->string index) index)
          (kernel (+ index 1))))
      ; Check the table has grown
      (check-true (> (vector-ref ht 1) vec-size))
      ;Check that every element is in the table
      (let kernel ([index 0])
        (unless (> index rehash-size)
          (check-equal? index
                        (find ht (number->string index)))
          (kernel (+ index 1))))))))

(define tests-update!
  (test-suite
   "Tests of update!"
   (test-case
    "Can update an exisitng key"
    (let ([ht (hash-table-new)])
      (add! ht "the key" 1729)
      (update! ht "the key" "the value (we're very creative, Sam.)")
      (check-equal? "the value (we're very creative, Sam.)"
                    (find ht "the key"))))
   (test-case
    "Cannot update a non exisiting key"
    (let ([ht (hash-table-new)])
      (check-exn exn:fail? (lambda () (update! ht "a" 'a)))))))

(define tests-find
  (test-suite
   "Tests of find"
   (test-case
    "Finds the correct element"
    (let ([ht (hash-table-new)])
      (add! ht "key" 1)
      (add! ht "eyk" 2)
      (add! ht "yke" 3)
      (check-equal? 1 (find ht "key"))
      (check-equal? 2 (find ht "eyk"))
      (check-equal? 3 (find ht "yke"))))
   (test-case
    "Cannot find a non-existing key"
    (let ([ht (hash-table-new)])
      (check-false (find ht "KEY"))))))

(define tests-delete!
  (test-suite
   "Tests of delete!"
   (test-case
    "Can delete an exisitng key"
    (let ([ht (hash-table-new)])
      (add! ht "do re mi" "abc")
      (add! ht "fa sol la" "cba")
      (delete! ht "do re mi" "abc")
      (check-false (find ht "do re mi"))
      ; Check number of pairs has decreased
      (check-equal? (vector-ref ht 0) 1)))
   (test-case
    "Cannot delete a non exisiting key"
    (let ([ht (hash-table-new)])
      (check-exn exn:fail? (lambda () (delete! ht "a" 'val)))))))

(define tests-all
  (test-suite
   "Tests the interactions between add!, update!, find, and delete!"
   (test-case
    "Can add, modify, read and delete the same key"
    (let ([ht (hash-table-new)])
      (add! ht "key" 'val)
      (add! ht "llave" "ya ve")
      (update! ht "llave" "ya ve")
      (check-equal? (find ht "llave") "ya ve")
      (delete! ht "llave" "ya ve")
      (check-false (find "llave"))))
   (test-case
    "Cannot delete a non exisiting key"
    (let ([ht (hash-table-new)])
      (check-exn exn:fail? (lambda () (delete! ht "a" 'val)))))))

; Run tests
(run-tests tests-hash)
(run-tests tests-add!)
(run-tests tests-update!)
(run-tests tests-find)
(run-tests tests-delete!)