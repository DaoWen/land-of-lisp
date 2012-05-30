(ns land-of-lisp.test.core
  (:use [land-of-lisp.core]
        [clojure.test]))

(defparam *test-val* 5)

(deftest mutation-test
  (is (= *test-val* 5))
  (setf *test-val* "five")
  (is (= *test-val* "five")))
