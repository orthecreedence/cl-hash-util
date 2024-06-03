(defpackage :cl-hash-util-test
  (:use :cl :fiveam :cl-hash-util))

(in-package :cl-hash-util-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-suite cl-hash-util-test))

(in-suite cl-hash-util-test)

(defparameter ht (hash ('a (hash ('b 3)))))

(test hget
  (is (= 3 (hget ht '(a b))))
  (is-false (hash-get ht '(c d)))
  (signals simple-error
    (let ((*error-on-nil* t))
      (hget ht '(c d))))
  (is (= 3 (let ((*error-on-nil* t))
             (hget ht '(a b)))))
  (signals simple-error (setf (hget ht '(b c)) t)))

(test setf-hget
  (is (= 4 (setf (hash-get ht '(a c)) 4)))
  (is (= 4 (hget ht '(a c))))
  (signals simple-error
    (setf (hget ht '(c d)) 5))
  (signals simple-error
    (let ((*error-on-nil* t))
      (setf (hget ht '(c d)) 5)))
  (is (equal '(b c d) (nth-value 2 (hget ht '(b c d)))))
  (is (= 5 (setf (hget ht '(b c d) :fill-func #'make-hash-table) 5)))
  (is (= 5 (hget ht '(b c d)))))

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
