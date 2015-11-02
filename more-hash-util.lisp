;;;Hash utilities contributed by Ben McGunigle <bnmcgn@gmail.com>.

(in-package :cl-hash-util)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symbolize (entity &key (package *package*))
  (if (symbolp entity)
      (intern (mkstr entity) package)
      (intern (string-upcase entity) package)))

(defmacro with-keys (keys hash-table &body body)
  (let ((htsym (gensym)))
    `(let ((,htsym ,hash-table))
       (symbol-macrolet
           ,(mapcar
             (lambda (entry)
               (let ((sym (if (listp entry)
                              (car entry)
                              (symbolize entry)))
                     (key (if (listp entry)
                              (second entry)
                              entry)))
                 `(,sym (gethash ,(if (and (symbolp key) (not (keywordp key)))
                                      (quote key)
                                      key)
                                 ,htsym))))
             keys)
         ,@body))))

(defun %hash-collecting-modes (mode)
  (typecase mode
    (keyword
     (case mode
       (:replace (list
                  (lambda (e n) (declare (ignore e)) n)
                  (lambda (v) v)))
        (:keep (list
                (lambda (e n) (declare (ignore n)) e)
                (lambda (v) v)))
        (:tally (list
                 (lambda (e n) (declare (ignore n)) (1+ e))
                 (lambda (v) (declare (ignore v)) 1)))
        (:sum (list
               (lambda (e n) (+ e n))
               (lambda (v) v)))
        (:append (list
                  (lambda (e n) (nconc e (list n)))
                  (lambda (v) (list v))))
        (:push (list
                (lambda (e n) (cons n e))
                (lambda (v) (list v))))
        (otherwise (error "Mode not found"))))
    (function
     (list
      (lambda (e n) (funcall mode e n))
      (lambda (v) v)))
    (list
     (unless (and (functionp (car mode)) (functionp (second mode)))
       (error "Invalid hash mode: needs two functions.")))))

(defmacro collecting-hash-table ((&key (test '(function eql) test-set-p)
                                    existing (mode :append)) &body body)
  (and test-set-p existing
       (error "Can't set test when reusing an existing hash table"))
  (let ((fill (gensym))
        (init (gensym))
        (stor (gensym))
        (xist (gensym)))
    `(let* ((,xist ,existing)
            (,stor (if ,xist ,xist (make-hash-table :test ,test))))
       (destructuring-bind (,fill ,init) (%hash-collecting-modes ,mode)
         (labels
             ((collect (key value &key mode)
                (destructuring-bind (fill init)
                    (if mode (%hash-collecting-modes mode) (list ,fill ,init))
                  (multiple-value-bind (itm exists) (gethash key ,stor)
                    (if exists
                        (setf (gethash key ,stor) (funcall fill itm value))
                        (setf (gethash key ,stor) (funcall init value)))))))
           ,@body
           ,stor)))))

;;;FIXME: Consider adding :mode thing to alist->plist, plist->alist. Would make
;;;them slower and more complicated...

(defun alist->plist (alist)
  "Converts an alist to a plist"
  (let ((stor nil))
    (dolist (itm alist)
      (push (car itm) stor)
      (push (cdr itm) stor))
    (nreverse stor)))

(defun plist->alist (plist)
  "Converts a plist to an alist"
  (loop for (k v) on plist by #'cddr
        collect (cons (intern (symbol-name k)) v)))

(defun alist->hash (al &key (mode :replace) existing)
  "Converts an alist to a hash table.

The :existing keyword can be used to supply a hash table to which the contents
of the alist will be added. Otherwise, a new hash table is created.

Since alists can contain multiple entries for a given key, alist->hash has a
variety of accumulation modes to handle them. The accumulation mode can be
set with the :mode keyword. Available modes are :replace :keep :tally :sum
:append and :push. :replace is the default.

If a function is supplied instead of a recognized mode, then values will be
accumulated to each key as by a reduce of the function."
  (collecting-hash-table (:mode mode :existing existing)
    (dolist (x (reverse al))
      (collect (car x) (cdr x)))))

(defun plist->hash (plist &key (mode :replace) existing)
  "Converts a plist to a hash table.

The :existing keyword can be used to supply a hash table to which the contents
of the plist will be added. Otherwise, a new hash table is created.

Since plists can contain multiple entries for a given key, plist->hash has a
variety of accumulation modes to handle them. The accumulation mode can be
set with the :mode keyword. Available modes are :replace :keep :tally :sum
:append and :push. :replace is the default.

If a function is supplied instead of a recognized mode, then values will be
accumulated to each key as by a reduce of the function."
  (collecting-hash-table (:mode mode :existing existing)
    (loop for (k v) on plist by #'cddr
          do (collect k v))))

(defun hash->alist (hsh)
  "Converts a hash table to an alist"
  (let ((stor nil))
    (maphash
     (lambda (k v)
       (push (cons k v) stor))
     hsh)
    (nreverse stor)))

(defun hash->plist (hsh)
  "Converts a hash table to a plist"
  (let ((stor nil))
    (maphash
     (lambda (k v)
       (push k stor)
       (push v stor))
     hsh)
    (nreverse stor)))

