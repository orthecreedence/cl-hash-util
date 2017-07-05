(asdf:defsystem cl-hash-util-test
  :author "Andrew Danger Lyon <orthecreedence@gmail.com>"
  :license "MIT"
  :description "Tests for cl-hash-util."
  :depends-on (#:cl-hash-util
               #:fiveam)
  :serial t
  :components ((:file "tests")))
