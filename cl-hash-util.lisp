;;; cl-hash-util is a very basic library for dealing with CL's hash tables. The
;;; idea was spawned through working with enough JSON APIs and config files,
;;; causing a lot of headaches in the process. For instance, to get a value deep
;;; within a hash, you have to do:
;;;   (gethash "city" (gethash "location" (gethash "user" obj)))
;;;
;;; With cl-hash-util, you can write:
;;;   (hash-get obj ("user" "location" "city"))
;;;
;;; hash-get can also deal with getting elements out of lists and arrays:
;;;   (hash-get obj ("user" "friends" 0 "name"))
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
  (:export hash hash-get)
  (:nicknames :hu))
(in-package :cl-hash-util)

(defmacro hash (&rest pairs)
  "Create a hash from list pairs. Great for creating hashes on the fly without
  having to do any horrible syntax. For instance:
    (hash (\"name\" \"andrew\") (\"age\" 25))

  This macro can be used recursively as well:
    (hash (\"name\" \"andrew\")
          (\"bed time\" \"7pm\")
          (\"location\" (hash (\"city\" \"santa cruz\") (\"state\" \"california\"))))
  
  This saves a LOT of time typing out annoying syntax."
  (let ((hash (gensym)))
    `(let ((,hash (make-hash-table :test #'equal)))
       (dolist (pair (list ,@(loop for p in pairs collect `(list ,(nth 0 p) ,(nth 1 p)))))
         (setf (gethash (nth 0 pair) ,hash) (nth 1 pair)))
       ,hash)))

(defmacro hash-get (obj path)
  "Allows you to specify a path to get values out of a hash/list object. For
  instance, if you did:

  (let ((myhash (hash (\"lol\" '(3 4 5)))))
    (hash-get myhash (\"lol\" 1)))

  which would return 4 (1st index of list stored under key 'lol of the hash
  table. Simplifies traversing responses from decoded JSON objects by about a
  trillion times."
  (let ((gets obj))
    (dolist (entry path)
      (setf gets (if (numberp entry)
                     `(elt ,gets ,entry)
                     `(gethash ,entry ,gets))))
    gets))

