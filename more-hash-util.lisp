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
               (let ((sym (symbolize (if (listp entry)
                                         (car entry)
                                         entry)))
                     (key (if (listp entry)
                              (second entry)
                              entry)))
                 `(,sym (gethash ,(if (and (symbolp key) (not (keywordp key)))
                                      (quote key)
                                      key)
                                 ,htsym))))
             keys)
         ,@body))))
