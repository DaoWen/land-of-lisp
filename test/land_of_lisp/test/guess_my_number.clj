(ns land-of-lisp.test.guess-my-number
  (:use [land-of-lisp.guess-my-number]
        [clojure.test]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 2: Guess My Number

(deftest play-two-games
  (doseq [i (range 2)]
    (is (= (guess-my-number) 50))
    (is (= (smaller) 25))
    (is (= (smaller) 12))
    (is (= (bigger)  18))
    (is (= (bigger)  21))
    (is (= (bigger)  23))
    (is (= (start-over) 50))))
