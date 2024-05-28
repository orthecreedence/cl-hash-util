(asdf:defsystem cl-hash-util-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :description "Tests for cl-hash-util."
  :depends-on (#:cl-hash-util
               #:fiveam)
  :serial t
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call '#:fiveam '#:run!
                                           (uiop:find-symbol* '#:cl-hash-util-test
                                                              '#:cl-hash-util-test)))
  :components ((:file "tests")))
