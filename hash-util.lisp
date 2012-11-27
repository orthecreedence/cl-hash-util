;;; cl-hash-util is a very basic library for dealing with CL's hash tables. The
;;; idea was spawned through working with enough JSON APIs and config files,
;;; causing a lot of headaches in the process. For instance, to get a value deep
;;; within a hash, you have to do:
;;;   (gethash "city" (gethash "location" (gethash "user" obj)))
;;;
;;; With cl-hash-util, you can write:
;;;   (hash-get obj '("user" "location" "city"))
;;;
;;; hash-get can also deal with getting elements out of lists and arrays:
;;;   (hash-get obj '("user" "friends" 0 "name"))
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
           hash-get
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

(defun hash-get (obj path)
  "Allows you to specify a path to get values out of a hash/list object. For
  instance, if you did:

    (let ((myhash (hash '(\"lol\" '(3 4 5)))))
      (hash-get myhash '(\"lol\" 1)))

  which would return 4 (1st index of list stored under key 'lol of the hash
  table). Simplifies traversing responses from decoded JSON objects by about a
  trillion times."
  (let ((placeholder obj))
    (dolist (entry path)
      (setf placeholder (if (numberp entry)
                            (elt placeholder entry)
                            (gethash entry placeholder))))
    placeholder))

(defun (setf hash-get) (val obj path)
  "Defines a setf for the hash-get function. Uses hash-get to get all but the
  last item in the path, then setfs that last object (either a gethash or an
  elt)."
  (let ((last-obj (hash-get obj (butlast path)))
        (final (car (last path))))
    (if (numberp final)
        (setf (elt last-obj final) val)
        (setf (gethash final last-obj) val))
    val))

(defun hash-copy (hash &key (test #'equal))
  "Performs a shallow (non-recursive) copy of a hash table."
  (let ((new-hash (make-hash-table :test test :size (hash-table-count hash))))
    (loop for k being the hash-keys of hash
          for v being the hash-values of hash do
          (setf (hash-get new-hash (list k)) (hash-get hash (list k))))
    new-hash))

