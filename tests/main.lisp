(defpackage cat-parser/tests/main
  (:use :cl
        :cat-parser
        :rove))
(in-package :cat-parser/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cat-parser)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
