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

(defun alist->plist (alist)
  "Converts an alist to plist."
  (let ((keyword-package (find-package :keyword)))
    (loop for i in alist
          collect (if (symbolp (car i))
                      (intern (symbol-name (car i)) keyword-package)
                      (intern (string-upcase (car i)) keyword-package))
          collect (cdr i))))

(defun plist->alist (plist)
  (loop for (k v) on plist by #'cddr
        collect (cons (intern (symbol-name k)) v)))

(defun alist->hash (al &key (mode :replace) existing)
  (collecting-hash-table (:mode mode :existing existing)
    (dolist (x (reverse al))
      (collect (car x) (cdr x)))))

(defun hash->alist (hsh)
  (cl-utilities:collecting
    (maphash
     (lambda (k v)
       (cl-utilities:collect (cons k v)))
     hsh)))

(defun hash->plist (hsh)
  (cl-utilities:collecting
    (maphash
     (lambda (k v)
       (cl-utilities:collect k)
       (cl-utilities:collect v))
     hsh)))

