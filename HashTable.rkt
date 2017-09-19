#lang racket

;vector ((key.val) par #, (vector-length hash-table), hash-table)

;overarching vector contains size of hash table (length of vector), number of elements in hash table, hash table stored in a vector
;hash table (vector) contains lists
;lists contain pairs


;pair = (cons key val)
;(car pair = key)
;(cdr pair = val)



;(define search
;  (lambda (key val vector replace?)
;    (let* ([index (hash-function key 0 0 vector)]
;           [lst (vector-ref(index))])
;    (if (replace?) ;if we call update, we want to replace
;        (vector-set! vector ;set vector[index] to new list containing replaced (key.val) pair
;                     index
;                     (cons (cons key val) ;create (key.val2) pair
;                           (remove (cons key val) lst))) ;attach (key.val2) pair to list with (key.val1) removed
;
;        ;;;(assoc x y)
;        (let kernel ([sub-lst lst]) ;sub recursive function
;          (if (null? lst)
;              null
;              (if (equal? (car (car lst)) key) ;if str matches existing key in lst, return (key.val) pair
;                  (car lst)
;                  (kernel (cdr sub-lst))))) ;otherwise, keep searching
;        ))))

;GLOBAL VALUES
(define alpha 52) ;capital and lowercase letters
(define percent-full .5) ;grow and rehash at 50% capacity

;PRACTICE EXAMPLE STRUCTURES
(define ex-lst (list (cons "one" 11)
                     (cons "two" 12)
                     (cons "three" 13)))
(define l (list (cons "two" 5)))

(define ex-vec (vector 3
                       0
                       ;(vector ex-lst)))
                       (make-vector 10 null)))
(vector-set! ex-vec 1 (vector-length (vector-ref ex-vec 2))) ;store length of vector stored at index 2
(vector-set! (vector-ref ex-vec 2) 0 ex-lst)
(vector-set! (vector-ref ex-vec 2) 3 l)

;;; Procedure:
;;;   hash-table-new
;;; Purpose:
;;;   Create a new hash-table.
;;; Produces:
;;;   hash-table, a hash-table
(define hash-table-new
  (lambda ()
    (vector 0 31 (make-vector 31 (list)))))

;;; Procedure:
;;;   hash
;;; Parameters:
;;;   key, a string
;;; Purpose:
;;;   Hashes the given key (string) to a unique index within the vector
;;; Produces:
;;;   The index of vector at which key will be stored
;;; Pre-conditions:
;;;   key contains characters from ASCII table (valued 0 to 127)
(define hash
  (let ([ALPHA 52] ;capital and lowercase letters
        [PERCENT-FULL .5]) ;grow and rehash at 50% capacity
    (lambda (key)
      (let kernel ([i 0]
                   [sum 0])
        (if (= i (string-length key))
            sum ;make sure index returned is within bounds of vector
            (kernel (+ i 1) ;increment character being examined in key
                    (+ sum (* (expt ALPHA
                                    (- (string-length key) (+ i 1)))
                              (char->integer (string-ref key i)))))))))) ;convert character in key at index i to ASCII value

;;; Procedure:
;;;   expand?
;;; Parameters:
;;;   vector, a vector containing <number of elements in hash table, size of hash
;;;           table (length of vector), a vector representing a hash table>
;;; Purpose:
;;;   Determines if hash table needs to be expaned based on max capacity (e.g. 50%)
;;; Produces:
;;;   A boolean: #t if hash table needs to grow; #f if hash table is not at max capacity
;;; Pre-conditions:
;;;   vector has required fields filled with valid input
(define expand?
  (lambda (vector)
    (if (>= (exact->inexact (/ (vector-ref vector 0)
                               (vector-ref vector 1)))
            percent-full) ;percent-full is previously defined
        #t
        #f)))

;;; Procedure:
;;;   remove-element
;;; Parameters:
;;;   key, a string
;;;   val, any type
;;;   lst, a list containing pairs
;;;   val?, a boolean
;;; Purpose:
;;;   Removed a specified (key.val) pair, if possible
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

(define add!
  (lambda (hash-table key value)
    (let* ([vec (vector-ref hash-table 1)]
           [vec_size (vector-length (vector-ref hash-table 1))]
           [idx (modulo (hash key) vec_size)]
           [bucket (vector-ref vec idx)])
      ; TODO: rehash if more than 50%
      ; assoc checks if key is the car of some pair in the list
      (if (assoc key bucket)
          (raise-arguments-error 'add!
                                 "Can not add a value if the key is already in the hash table."
                                 "key" key
                                 "value" value)
          ;Add pair to front of list
          (vector-set! vec idx (cons (cons key value) bucket)))
      ; Increment size
      (vector-set! hash-table 0 (+ vec_size 1)))))


;;; Procedure:
;;;   update
;;; Parameters:
;;;   key, a string
;;;   val, any type
;;;   vector, a vector containing <number of elements in hash table, size of hash
;;;           table (length of vector), a vector representing a hash table>
;;; Purpose:
;;;   Updates existing (key.val') pair in hash table with specified (key.val)
;;; Produces:
;;;   A vector with the specified pair containing key updated in hash table or an error if key
;;;   does not exist in hash table
;;; Pre-conditions:
;;;   vector has required fields filled with valid input
(define update
  (lambda (key val vector)
           ;finds index in hash table, stored in vector
    (let* ([index (hash key)]
           ;list stored in hash table at index
           [lst (vector-ref (vector-ref vector 2) index)])
      (vector-set!
       (vector-ref vector 2) ;field of vector holding hash table
       index
       ;add (key.val) pair to list at index of hash table after removing the existing
       ;pair stored under key
       (cons (cons key val)
             (remove-element key val lst #f)))
      vector)))

;;; Procedure:
;;;   delete
;;; Parameters:
;;;   key, a string
;;;   val, any type
;;;   vector, a vector containing <number of elements in hash table, size of hash
;;;           table (length of vector), a vector representing a hash table>
;;; Purpose:
;;;   Removes (key.val) pair in hash table or returns error if (key.val) pair is not found
;;; Produces:
;;;   A vector with the specified (key.val) removed from hash table or an error if (key.val) pair
;;;   does not exist in hash table
;;; Pre-conditions:
;;;   vector has required fields filled with valid input
(define delete
  (lambda (key val vector)
           ;finds index in hash table, stored in vector
    (let* ([index (hash key)]
           ;list stored in hash table at index
           [lst (vector-ref (vector-ref vector 2) index)])
      (vector-set!
       (vector-ref vector 2)
       index
       (remove-element key val lst #t)))
    vector))

;;; Procedure:
;;;   add-to-table
;;; Parameters:
;;;   str, a string
;;;   value, any type
;;;   hash-table, a vector containing lists of (key.value) pairs
;;; Purpose:
;;;   Inserts a (key.value) pair into a hash table
;;; Produces:
;;;   A vector representing a hash table
(define add-to-table
  (lambda (str value hash-table)
    (let ([index (modulo (hash str) (vector-length hash-table))])
          (vector-set! hash-table
                       index
                       (cons (cons str value)
                             (vector-ref hash-table index))))))

;;; Procedure:
;;;   find
;;; Parameters:
;;;   str, a string
;;;   hash-vec, a vector containing <number of elements in hash table, size of
;;;             hash table (length of vector), a vector representing a hash table>
;;; Purpose:
;;;   Finds the value associated with a given key
;;; Produces:
;;;   The value associated with a given key, or null if a pair is not found
;;; Pre-conditions:
;;;   hash-vec has required fields filled with valid input
(define find
  (lambda (str hash-vec)
    (let* ([table (vector-ref hash-vec 2)]
           [index (modulo (hash str) (vector-length table))]
           [pair (assoc str (vector-ref table index))])
      (if (equal? pair #f)
          null
          (cdr pair)))))

;;; Procedure:
;;;   read-list
;;; Parameters:
;;;   lst, a list of (key.value) pairs
;;;   table, a vector
;;; Purpose:
;;;   Reads through a list of (key.value) pairs and rehashes each element based on key
;;; Produces:
;;;   A populated hash table
;;; Pre-conditions:
;;;   table is an empty vector
(define read-list
  (lambda (lst table)
    (if (null? lst)
        table
        (begin
          (add-to-table (car (car lst))
                        (cdr (car lst))
                        table)
          (read-list (cdr lst) table)))))

;;; Procedure:
;;;   read-table
;;; Parameters:
;;;   old-table, a vector
;;;   new-table, a vector
;;;   index, an integer
;;; Purpose:
;;;   Reads through a vector containing lists of (key.value) pairs and rehashes each element
;;;   within thoses lists based on keys
;;; Produces:
;;;   A populated hash table
;;; Pre-conditions:
;;;   new-table is an empty vector
;;;   index should initally be 0
(define read-table
  (lambda (old-table new-table index)
    (if (equal? index (vector-length old-table))
        new-table
        (begin
          (read-list (vector-ref old-table index)
                     new-table)
          (read-table old-table
                      new-table
                      (+ 1 index))))))

;;; Procedure:
;;;   rehash
;;; Parameters:
;;;   hash-vec, a vector containing <number of elements in hash table, size of
;;;             hash table (length of vector), a vector representing a hash table>
;;; Purpose:
;;;   Doubles size of an existing hash table and rehashes all the elements in original hash table
;;; Produces:
;;;   A freshly hashed hash table
;;; Pre-conditions:
;;;   hash-vec has required fields filled with valid input
(define rehash
  (lambda (hash-vec)
    (let* ([old-table (vector-ref hash-vec 2)]
          [size (vector-ref hash-vec 1)]
          [new-table (make-vector (* size 2) null)])
      (vector-set! hash-vec
                   2
                   (read-table old-table new-table 0)))))

(provide
  hash-table-new
    hash
         add!)
