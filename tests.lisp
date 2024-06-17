(defpackage :cl-hash-util-test
  (:use :cl :fiveam :cl-hash-util))

(in-package :cl-hash-util-test)

(def-suite cl-hash-util-test)

(in-suite cl-hash-util-test)

(test hget
  (let ((ht (hash ('a (hash ('b 3))))))
    (is (= 3 (hget ht '(a b))))
    (is-false (hash-get ht '(c d)))
    (signals simple-error
      (let ((*error-on-nil* t))
	(hget ht '(c d))))
    (is (= 3 (let ((*error-on-nil* t))
	       (hget ht '(a b)))))
    (signals simple-error (setf (hget ht '(b c)) t))))

(test setf-hget
  (let ((ht (hash ('a (hash ('b 3))))))
    (is (= 4 (setf (hash-get ht '(a c)) 4)))
    (is (= 4 (hget ht '(a c))))
    (signals simple-error
      (setf (hget ht '(c d)) 5))
    (signals simple-error
      (let ((*error-on-nil* t))
	(setf (hget ht '(c d)) 5)))
    (is (equal '(b c d) (nth-value 2 (hget ht '(b c d)))))
    (is (= 5 (setf (hget ht '(b c d) :fill-func #'make-hash-table) 5)))
    (is (= 5 (hget ht '(b c d))))))

(test hash-merge
  (let* ((hash-table-1
	  (hash-create (list (list "first" 1) (list "second" 2))))
	(hash-table-2
	  (hash-create (list (list "first" 234234) (list "third" 235346))))
	 (merged-hash-table
	   (hash-merge hash-table-1 hash-table-2))
	 (reverse-merged-hash-table
	   (hash-merge hash-table-2 hash-table-1)))
    (is (= 3 (hash-table-count merged-hash-table)))
    (is (= 1 (gethash "first" merged-hash-table)))
    (is (= 234234 (gethash "first" reverse-merged-hash-table)))))

(test with-keys
  (let ((hash-table-1 (hash-create (list (list "first" 1) (list "second" 2))))
	(hash-table-2 (hash-create (list (list "First" 1) (list "second" 2))))
	(hash-table-3 (hash-create (list (list 1 1) (list 2 2)))))
    (is (= 1
	   (with-keys ("first")
	       hash-table-1
	     first))
	"base case")
    (is (null
	 (with-keys ("First")
	     hash-table-1
	   first))
	"case sensitive")
    (is (= 1
	   (with-keys ("First")
	       hash-table-2
	     first))
	"case sensitive")
    (is (= 1
	   (with-keys ((number-one "first"))
	       hash-table-1
	     number-one))
	"supply symbol name")
    (is (= 1
	   (with-keys ((one 1))
	       hash-table-3
	     one))
	"works with numbers when given a symbol")
    (is (= 12345
	   (with-keys ((number-three "three" 12345))
	       hash-table-1
	     number-three))
	"default value")))
