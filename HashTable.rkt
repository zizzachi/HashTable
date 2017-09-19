#lang racket

;vector ((key.val) par #, (vector-length hash-table), hash-table)

;overarching vector contains size of hash table (length of vector), number of elements in hash table, hash table stored in a vector
;hash table (vector) contains lists
;lists contain pairs

;GLOBAL Constants
(define ALPHA 128) ;capital and lowercase letters
;(define PERCENT-FULL .5) ;grow and rehash at 50% capacity

;;; Procedure:
;;;   hash
;;; Parameters:
;;;   key, a string
;;; Purpose:
;;;   Hashes the given key (string) to an integer
;;; Produces:
;;;   hash, an integer
;;; Pre-conditions:
;;;   key contains characters from the ASCII table (valued 0 to 127)
;;;   ALPHA is defined
(define hash
    (lambda (key)
      (let kernel ([i 0]
                   [sum 0])
        (if (= i (string-length key))
            sum ;make sure index returned is within bounds of vector
            (kernel (+ i 1) ;increment character being examined in key
                    (+ sum (* (expt ALPHA
                                    (- (string-length key) (+ i 1)))
                              (char->integer (string-ref key i))))))))) ;convert character in key at index i to ASCII value

;;; Procedure:
;;;   hash-table-new
;;; Purpose:
;;;   Create a new hash-table.
;;; Produces:
;;;   hash-table, a hash-table
(define hash-table-new
  (lambda ()
    (vector 0 31 (make-vector 31 (list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for add! ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Procedure:
;;;   expand?
;;; Parameters:
;;;   ht, a hash table created with hash-table-new
;;; Purpose:
;;;   Determines if the hash table needs to be expaned based on max capacity (e.g. 50%)
;;; Produces:
;;;   A boolean: #t if hash table needs to grow; #f if hash table is not at max capacity
;;; Pre-conditions:
;;;   PERCENT-FULL is defined
(define expand?
  (lambda (ht)
    (let ([PERCENT-FULL .5]) ;grow and rehash at 50% capacity)
      (if (>= (exact->inexact (/ (vector-ref ht 0)
                                 (vector-ref ht 1)))
              PERCENT-FULL)
          #t
          #f))))

;;; Procedure:
;;;   add-to-table!
;;; Parameters:
;;;   key, a string
;;;   value, any type
;;;   table, a vector containing lists of (key.value) pairs
;;; Purpose:
;;;   Inserts a (key.value) pair into a hash table
;;; Produces:
;;;   Nothing, called for side effect
;;; Pre-conditions:
;;;   [No additional]
;;; Post-conditions:
;;;   table has been mutated
(define add-to-table!
  (lambda (key val table)
    (let ([index (modulo (hash key) (vector-length table))])
      (vector-set! table
                   index
                   (cons (cons key val)
                         (vector-ref table index))))))

;;; Procedure:
;;;   rehash-list!
;;; Parameters:
;;;   lst, a list of (key.value) pairs
;;;   table, a vector
;;; Purpose:
;;;   Reads through a list of (key.value) pairs and rehashes each element based on key,
;;;     placing each pair into table according to the hash value
;;; Produces:
;;;   A populated hash table
;;; Pre-conditions:
;;;   [No additional]
;;; Post-conditions:
;;;   table has been mutated
(define rehash-list!
  (lambda (lst table)
    (if (null? lst)
        table
        (begin
          (add-to-table! (car (car lst))
                         (cdr (car lst))
                         table)
          (rehash-list! (cdr lst)
                        table)))))

;;; Procedure:
;;;   rehash-table!
;;; Parameters:
;;;   old-table, a vector containing lists of (key.value) pairs
;;;   new-table, a vector
;;;   index, an integer
;;; Purpose:
;;;   Reads through a vector containing lists of (key.value) pairs and rehashes each element
;;;     within those lists based on their keys, placing them into a new vector
;;; Produces:
;;;   A populated hash table
;;; Pre-conditions:
;;;   new-table is an empty vector
;;;   index should initally be 0
(define rehash-table!
  (lambda (old-table new-table index)
    (if (equal? index (vector-length old-table))
        new-table
        (begin
          (rehash-list! (vector-ref old-table index)
                        new-table)
          (rehash-table! old-table
                         new-table
                         (+ 1 index))))))

;;; Procedure:
;;;   rehash!
;;; Parameters:
;;;   ht, a hash table created with hash-table-new
;;; Purpose:
;;;   Doubles the size of an existing hash table and rehashes all the elements in the
;;;     original hash table into the new table.
;;; Produces:
;;;   Nothing, called for side effect
;;; Pre-conditions:
;;;   [No additional]
;;; Post-conditions:
;;;   ht has been mutated
(define rehash!
  (lambda (ht)
    (let* ([size (vector-ref ht 1)]
           [old-table (vector-ref ht 2)]
           [new-table (make-vector (+ (* size 2) 1) (list))])
      (vector-set! ht
                   2
                   (rehash-table! old-table new-table 0)))))

;;; Procedure:
;;;   add!
;;; Parameters:
;;;   ht, a hash table created with hash-table-new
;;;   key, a string
;;;   val, any type
;;; Purpose:
;;;   Adds a new (key.val) pair into the hash table
;;; Produces:
;;;   Nothing, called for side-effect.
;;; Pre-conditions:
;;;   key is a string
;;; Post-conditions:
;;;   ht has been mutated so that the given key is mapped to the given val
;;;   add! raises an error if key is already in the hash table
(define add!
  (lambda (ht key val)
    (let* ([table (vector-ref ht 2)]
           [table-size (vector-ref ht 1)]
           [num-pairs (vector-ref ht 0)]
           [index (modulo (hash key) table-size)]
           [lst (vector-ref table index)])
      ; TODO: rehash if more than 50%
      ; assoc checks if key is the car of some pair in the list
      (if (assoc key lst)
          (raise-arguments-error 'add!
                                 "Can not add a value if the key is already in the hash table."
                                 "key" key
                                 "value" val)
          ;Add pair to front of list
          (vector-set! table index (cons (cons key val) lst)))
      ; Increment size
      (vector-set! ht 0 (+ num-pairs 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helper functions for update! and delete! ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Procedure:
;;;   remove-element
;;; Parameters:
;;;   key, a string
;;;   val, any type
;;;   lst, a list containing pairs
;;;   val?, a boolean
;;; Purpose:
;;;   Removes a specified (key.val) pair, if possible
;;; Produces:
;;;   A list with the specified element removed or an error if the key (and maybe value,
;;;   if val? is #t) is not found
;;; Pre-conditions:
;;;   If val of (key.val) needs to be considered to determine if a (key.val) pair has
;;;   been found within lst, val? should be #t
(define remove-element
  (lambda (key val lst val?)
    (cond [(null? lst)
           (raise-arguments-error 'Nonexistent
                                  "(key.value) pair was not found within list"
                                  "key" key
                                  "value" val)]
          [(if val?
               ;if we care about val, make sure both key and val match
               (if (and (equal? (car (car lst)) key) (= (cdr (car lst)) val))
                   ;if they match, return remainder of list
                   (cdr lst)
                   ;build a list of non-matching elements
                   (cons (car lst)
                         (remove-element key val (cdr lst) val?)))

               ;if we don't care about val, only make sure val matches
               (if (equal? (car (car lst)) key)
                   ;if it matches, return remainder of list
                   (cdr lst)
                   ;build a list of non-matching elements
                   (cons (car lst)
                         (remove-element key val (cdr lst) val?))))])))

;;; Procedure:
;;;   update!
;;; Parameters:
;;;   ht, a hash table created with hash-table-new
;;;   key, a string
;;;   val, any type
;;; Purpose:
;;;   Updates existing (key.val) pair in hash table with specified (key.val)
;;; Produces:
;;;   Nothing, called for side-effect.
;;; Pre-conditions:
;;;   key is a string
;;; Post-conditions:
;;;   ht has been mutated so that the given key is now mapped to the given val
;;;   update! raises an error if key is not in the hash table
(define update!
  (lambda (ht key val)
    ;finds index in hash table, stored in vector
    (let* ([index (modulo (hash key) (vector-ref ht 1))]
           [table (vector-ref ht 2)]
           ;list stored in hash table at index
           [lst (vector-ref table index)])
      ;add (key.val) pair to list at index of hash table after removing the existing
      ;pair stored under key
      (vector-set! table
                   index
                   (cons (cons key val)
                         (remove-element key val lst #f))))))

;;; Procedure:
;;;   delete!
;;; Parameters:
;;;   ht, a hash table created with hash-table-new
;;;   key, a string
;;;   val, any type
;;; Purpose:
;;;   Removes (key.val) pair in hash table or returns error if (key.val) pair is not found
;;; Produces:
;;;   A vector with the specified (key.val) removed from hash table or an error if (key.val) pair
;;;   does not exist in hash table
;;; Pre-conditions:
;;;   vector has required fields filled with valid input
(define delete!
  (lambda (ht key val)
    ;finds index in hash table, stored in vector
    (let* ([index (modulo (hash key) (vector-ref ht 1))]
           [table (vector-ref ht 2)]
           ;list stored in hash table at index
           [lst (vector-ref table index)])
      ; TODO decrement num pairs
      (vector-set! table
                   index
                   (remove-element key val lst #t)))))

;;; Procedure:
;;;   find
;;; Parameters:
;;;   ht, a hash table created with hash-table-new
;;;   key, a string
;;; Purpose:
;;;   Finds the value associated with a given key
;;; Produces:
;;;   The value associated with a given key, or null if a pair is not found
;;; Pre-conditions:
;;;   [No additional]
(define find
  (lambda (ht key)
    (let* ([table (vector-ref ht 2)]
           [index (modulo (hash key) (vector-length table))]
           [pair (assoc key (vector-ref table index))])
      (if pair
          (cdr pair)
          null))))

; Export procedures for unit tests
(provide
 hash
 hash-table-new
 add!
 update!
 find
 delete!)
