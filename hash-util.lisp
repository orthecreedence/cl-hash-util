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
  (:export hash-create
           hash
           hget
           hash-copy)
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

(defun hget (obj path)
  "Allows you to specify a path to get values out of a hash/list object. For
   instance, if you did:

     (let ((myhash (hash '(\"lol\" '(3 4 5)))))
       (hget myhash '(\"lol\" 1)))

   which would return 4 (1st index of list stored under key 'lol of the hash
   table). Simplifies traversing responses from decoded JSON objects by about a
   trillion times."
  (let ((placeholder obj))
    (dolist (entry (if (listp path) path (list path)))
      (setf placeholder (if (numberp entry)
                            (elt placeholder entry)
                            (gethash entry placeholder))))
    placeholder))

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

(defun hash-copy (hash &key (test #'equal))
  "Performs a shallow (non-recursive) copy of a hash table."
  (let ((new-hash (make-hash-table :test test :size (hash-table-count hash))))
    (loop for k being the hash-keys of hash
          for v being the hash-values of hash do
          (setf (gethash k new-hash) v))
    new-hash))

