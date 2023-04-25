(defpackage mahjong/tests/main
  (:use :cl
        :mahjong
        :rove))
(in-package :mahjong/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :mahjong)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
