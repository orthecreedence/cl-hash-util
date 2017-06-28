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
  (:export :*error-on-nil*
           :hash-create
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

(defvar *error-on-nil* nil
  "If hget encounters a nil, error out.")

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

(defun %hget-access (obj key)
  (if (hash-table-p obj)
      (gethash key obj)
      (handler-case
          (values (elt obj key) t)
        (t (e) (declare (ignore e)) (values nil nil)))))

(defun %store-p (obj key)
  "Is the object something from which key could be fetched?"
  (or (hash-table-p obj)
      (and (numberp key)
           (or (listp obj)
               (vectorp obj)))))

(defun %find-lowest-store (obj path)
  (let ((placeholder obj))
    (loop for entries on path
       do
         (if placeholder
             (if (cdr entries)
                 (if (%store-p placeholder (car entries))
                     (let ((next (%hget-access placeholder (car entries))))
                       (unless next
                         (return-from %find-lowest-store
                           (values placeholder entries))) ;Partial
                       (setf placeholder next)) ;Try some more
                     (error "Can't descend into tree. Value is not null, but is not a hash table or sequence."))
                 (return-from %find-lowest-store
                   (values placeholder (last path)))) ;Total success!
             (return-from %find-lowest-store (values nil path)))))) ;Initial nil

(defun hget (obj path)
  "Allows you to specify a path to get values out of a hash/list object. For
   instance, if you did:

     (let ((myhash (hash '(\"lol\" '(3 4 5)))))
       (hget myhash '(\"lol\" 1)))

   which would return 4 (1st index of list stored under key 'lol of the hash
   table). Simplifies traversing responses from decoded JSON objects by about a
   trillion times.

   The second and third values returned by hget indicate how much success it had
   in looking up your request. If the second value is T, everything was found,
   right up to the end node. When the second value is NIL, the third value is
   the portion of the supplied path that was missing.

   By setting *error-on-nil* to true, hget can be persuaded to throw an error if
   any of the upper part of the tree is missing . It will not throw an error if
   the final value is not set."
  (multiple-value-bind (lstore leftover) (%find-lowest-store obj path)
    (let ((leftlen (length leftover)))
      (cond
        ((= 0 leftlen) (error "0 length path."))
        ((= 1 leftlen)
         (multiple-value-bind (val sig) (%hget-access lstore (car (last path)))
           (if sig
               (values val t)
               (values nil nil leftover))))
        (t
         (if *error-on-nil*
             (if (numberp (car leftover))
                 (error "NIL found instead of sequence")
                 (error "NIL found instead of hash table"))
             (values nil nil leftover)))))))

(defun %hget-set (store key value)
  (if (hash-table-p store)
      (setf (gethash key store) value)
      (setf (elt store key) value)))

(defun (setf hget) (val obj path &key fill-func)
  "Defines a setf for the hget function. Uses hget to get all but the
   last item in the path, then setfs that last object (either a gethash or an
   elt).

   If any of the path aside from the last item is missing, it will throw an
   error. To change this behavior, supply an object construction function with
   the :fill-func parameter. (Setf hget) will fill out the tree up to the last
   path item with the objects that this function returns."
  (let ((path (if (listp path) path (list path))))
    (multiple-value-bind (lstore leftover) (%find-lowest-store obj path)
      (unless lstore
        (error "Can't set: No fill-func and no hash-table or sequence found."))
      (when (> (length leftover) 1)
        (if fill-func
            (dolist (key (butlast leftover))
              (let ((stor (funcall fill-func)))
                (%hget-set lstore key stor)
                (setf lstore stor)))
            (error
             "Can't set: Missing storage objects in tree and no fill-func supplied.")))
      (%hget-set lstore (car (last leftover)) val))))

(setf (fdefinition 'hash-get) #'hget)
(setf (fdefinition '(setf hash-get)) (fdefinition '(setf hget)))

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
