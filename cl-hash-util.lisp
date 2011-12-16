(defpackage :cl-hash-util
  (:use :cl :cl-user)
  (:export hash hash-get)
  (:nicknames :hu))
(in-package :cl-hash-util)

(defun clsym (obj)
  "Convert a symbol to a string. If not a symbol, don't do anything."
  (if (symbolp obj)
      (string-downcase (write-to-string symbol))
      obj))

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
    (hash-get myhash (lol 1)))

  which would return 4 (1st index of list stored under key 'lol of the hash
  table. Simplifies traversing responses from decoded JSON objects by about a
  trillion times."
  (let ((path (reverse path)))
    (labels ((trav-r (obj path)
               (let ((el (car path))
                     (next (cdr path)))
                 (cond ((or (equal (list el) next) (eql next nil))
                        (if (numberp el)
                            `(elt ,obj ,el)
                            `(gethash ,(clsym el) ,obj)))
                       ((numberp el)
                        `(elt ,(trav-r obj next) ,el))
                       (t
                        `(gethash ,(clsym el) ,(trav-r obj next)))))))
      (trav-r obj path))))

