;;; cl-hash-util is a very basic library for dealing with CL's hash tables. The
;;; idea was spawned through working with enough JSON APIs and config files,
;;; causing a lot of headaches in the process. For instance, to get a value deep
;;; within a hash, you have to do:
;;;   (gethash "city" (gethash "location" (gethash "user" obj)))
;;;
;;; With cl-hash-util, you can write:
;;;   (hget obj '("user" "location" "city"))
;;;
;;; hget can also deal with getting elements out of lists and arrays:
;;;   (hget obj '("user" "friends" 0 "name"))
;;;
;;; which normally would have to be written as such:
;;;   (gethash "name" (elt (gethash "friends" (gethash "user" obj)) 0))
;;;
;;; ...uuuugly.
;;;
;;; cl-hash-util also provides an easy way to build hash tables on the fly.
;;; Where you'd normally have to do something like:
;;;   (let ((myhash (make-hash-table :test #'equal)))
;;;     (setf (gethash "name" myhash) "andrew")
;;;     (setf (gethash "location" myhash) "santa cruz")
;;;     myhash)
;;;
;;; You can now do:
;;;   (hash-create '(("name" "andrew") ("location" "santa cruz")))
;;;
;;;   -OR-
;;;
;;;   (hash ("name" "andrew") ("location" "santa cruz"))
;;;
;;; You can also do nested hashes:
;;;   (hash ("name" "andrew")
;;;         ("location" (hash ("city" "santa cruz")
;;;                           ("state" "CA"))))
;;;
;;; This saves a lot of typing =].
;;;
;;; The project is hosted on github:
;;;   https://github.com/orthecreedence/cl-hash-util

(defpackage :cl-hash-util
  (:use :cl :cl-user)
  (:export :hash-create
           :hash
           :hget
           :hash-copy
           :hash-keys
           :with-keys
           :collecting-hash-table
           :it
           :collect
           :alist->plist
           :plist->alist
           :alist->hash
           :plist->hash
           :hash->alist
           :hash->plist
           :hget/extend
           :hash-get
           :hash-get/extend)
  (:nicknames :hu))
(in-package :cl-hash-util)

(defun hash-create (pairs &key (test #'equal))
  "Create a hash table with limited syntax:

     (hash `((\"name\" \"andrew\") (\"city\" \"santa cruz\")))

   which would otherwise be:

     (let ((hash (make-hash-table :test #'equal)))
       (setf (gethash \"name\" hash) \"andrew\")
       (setf (gethash \"city\" hash) \"santa cruz\")
       hash)

   yuck city."
  (let ((hash (make-hash-table :test test)))
    (dolist (pair pairs)
      (setf (gethash (car pair) hash) (cadr pair)))
    hash))

(defmacro hash (&rest pairs)
  "Extends hash-create syntax to make it nicer."
  `(hash-create (list ,@(loop for pair in pairs collect (list 'list (car pair) (cadr pair))))))

(defun %hget-core (obj path)
  (let ((placeholder obj))
    (loop for entries on (if (listp path) path (list path))
       do
         (if (and entries placeholder
                  (not (or (hash-table-p placeholder)
                           (listp placeholder)
                           (vectorp placeholder))))
             (error "Can't descend into tree. Value is not null, but is not a hash table or sequence.")
             (let ((current (if (numberp (car entries))
                                (elt placeholder (car entries))
                                (gethash (car entries) placeholder))))
               (if current
                   (setf placeholder current)
                   (return-from %hget-core (values current entries))))))
    placeholder))

(defun hget (obj path)
  "Allows you to specify a path to get values out of a hash/list object. For
   instance, if you did:

     (let ((myhash (hash '(\"lol\" '(3 4 5)))))
       (hget myhash '(\"lol\" 1)))

   which would return 4 (1st index of list stored under key 'lol of the hash
   table). Simplifies traversing responses from decoded JSON objects by about a
   trillion times."
  (multiple-value-bind (value leftover) (%hget-core obj path)
    (format t "leftover: ~a~%" leftover)
    (if (< (length leftover) 1)
        (if (numberp (car leftover))
            (error "NIL found instead of sequence")
            (error "NIL found instead of hash table"))
        value)))

(defun (setf hget) (val obj path)
  "Defines a setf for the hget function. Uses hget to get all but the
   last item in the path, then setfs that last object (either a gethash or an
   elt)."
  (let ((path (if (listp path) path (list path))))
    (let ((last-obj (hget obj (butlast path)))
          (final (car (last path))))
      (if (numberp final)
          (setf (elt last-obj final) val)
          (setf (gethash final last-obj) val))
      val)))

(defun hget/extend (obj path)
  "Like hget, but does not raise an error if the lower part of the tree is
   missing. Instead, it returns nil as the first value and the unmatched
   portion of the path as the second value.

       (defvar *hash* (hash-create `((:a ,(hash-create `((:b ,(hash))))))))
       (hget/extend *hash* '(:a :b :c :d :e))

   will return the values NIL and (:C :D :E).

   On its own, hget/extend doesn't extend anything, used with setf, it will
   pad out a missing tree with new hash tables, placing the value in the
   bottom-most hash table."
  (multiple-value-bind (value leftover) (%hget-core obj path)
    (if leftover
        (values nil leftover)
        value)))

(defun (setf hget/extend) (val obj path
                           &optional (new-hash-func #'make-hash-table))
  "Setf for hget/extend. Obj is assumed to contain a tree of hash tables and
   sequences that contains the branches in path. If any nodes are absent, they
   will be padded out before setting the value in the bottom-most node. By
   default the padding consists of calls to make-hash-table with no arguments.
   An alternate hash table constructor function can be supplied with the
   new-hash-func argument.

   Hget/extend does not support the addition of sequences or arrays to the tree.
   They must be added manually. Numerical indices in the extension portion of
   the path will result in an error."
  (unless (functionp new-hash-func)
    (error "New-hash-func argument is a non-function"))
  (let ((path (if (listp path) path (list path))))
    (multiple-value-bind (value leftover) (%hget-core obj path)
      (if leftover
          (let ((current value))
            (when (some #'numberp leftover)
              (error "Number in hget/extend path list, but hget/extend won't create sequences"))
            (dolist (key (butlast leftover))
              (setf (gethash key current) (funcall new-hash-func))
              (setf current (gethash key current)))
            (setf (gethash (car (last leftover)) current) val))
          (setf (hget obj path) val)))))

(setf (fdefinition 'hash-get) #'hget)
(setf (fdefinition 'hash-get/extend) #'hget/extend)

(defun hash-copy (hash &key (test #'equal))
  "Performs a shallow (non-recursive) copy of a hash table."
  (let ((new-hash (make-hash-table :test test :size (hash-table-count hash))))
    (loop for k being the hash-keys of hash
          for v being the hash-values of hash do
            (setf (gethash k new-hash) v))
    new-hash))

(defun hash-keys (hash)
  "Grab all the hash keys of the passed hash into a list."
  (loop for x being the hash-keys of hash collect x))
