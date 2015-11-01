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

With-keys
---------(

With-keys is the hash-table equivalent of with-slots.

```common-lisp
(with-keys
   ("name" (loc "location"))
    (hash ("name" "andrew") ("location" "santa cruz"))
    (setf loc (string-upcase loc))
  (format nil "Hi, ~a in ~a!" name loc))
"Hi, andrew in SANTA CRUZ!"
```

The first parameter is a list of keys that with-keys will reference in the hash
table provided in the second parameter. With-keys will attempt to convert each
key into a symbol, binding the hash table value to it during body execution.
String keys are upcased before conversion to symbols.

If you don't want with-keys to guess at a symbol for a key, supply a list -
 (*symbol key*) - in place of the key, as in (loc "location") above.

Collecting-hash-table
---------------------

A collection macro that builds and outputs a hash-table. To add to the hash
table, call the collect function with a key and a value from within the scope
of the collecting-hash-table macro. The value will be inserted or combined with
existing values according to the specified mode.

This code collects words into bins based on their length:

```common-lisp
(collecting-hash-table (:mode :append)
  (dotimes (i 10)
    (let ((word (format nil "~r" i)))
      (collect (length word) word)))
```
Result: <hash table: 5 => ("three" "seven" "eight")
                     3 => ("one" "two" "six")
                     4 => ("zero" "four" "five" "nine")>

The mode can be set in the parameters section of collecting-hash-table with the
:mode keyword. The :mode keyword can also be passed to individual collect calls.


Keyword parameters:

:test - Test function parameter passed to make-hash-table when creating a new
hash-table

:existing - Pass an existing hash-table to the macro for modification. Using
this option at the same time as :test will result in an error.

:mode - Set the default mode for the collect function.

Modes
-----

:replace - Acts the same as (setf (gethash *key* *ht*) *value*), replacing any
existing value with the new one.

:keep - Only inserts the value if the key did not previously exist.

:tally - Ignores the input value, instead adding 1 to the key value.

:sum - Adds the input value to the key value. Input should be numeric.

:append - Appends the value to the list that is presumed to be under the key.
If the key doesn't yet exist, places the value in a new list.

:push - Like append, but sticks things on the other end.

Obviously, not all modes are compatible with each other. Collecting-hash-table
makes no attempt to save you from intermingling them.







