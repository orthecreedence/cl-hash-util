cl-hash-util
============
cl-hash-util is a very basic library for dealing with CL's hash tables. The
idea was spawned through working with enough JSON APIs and config files,
causing a lot of headaches in the process. For instance, to get a value deep
within a hash, you have to do:
```common-lisp
(gethash "city" (gethash "location" (gethash "user" obj)))
```

I find the inside-out approach unintuitive, regardless of how many lisp nerds
are going to yell at me about how it's correct.

With cl-hash-util, you can write:

```common-lisp
(hash-get obj '("user" "location" "city"))
```

hash-get can also deal with getting elements out of lists and arrays:

```common-lisp
(hash-get obj '("user" "friends" 0 "name"))
```

which normally would have to be written as such:

```common-lisp
(gethash "name" (elt (gethash "friends" (gethash "user" obj)) 0))
```

...uuuugly.

cl-hash-util also provides an easy way to build hash tables on the fly. 
Where you'd normally have to do something like:

```common-lisp
(let ((myhash (make-hash-table :test #'equal)))
  (setf (gethash "name" myhash) "andrew")
  (setf (gethash "location" myhash) "santa cruz")
  myhash)
```

You can now do:

```common-lisp
;; functional version
(hash-create '(("name" "andrew") ("location" "santa cruz")))

;; convenience macro `hash`
(hash ("name" "andrew") ("location" "santa cruz"))
```

You can also do nested hashes:
```common-lisp
(hash ("name" "andrew")
      ("location" (hash ("city" "santa cruz")
                        ("state" "CA"))))
```

This saves a lot of typing =].


