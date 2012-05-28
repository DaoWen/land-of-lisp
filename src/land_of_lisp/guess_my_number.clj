(ns land-of-lisp.guess-my-number
  (:use [land-of-lisp.core]
        [clojure.test]))

(def big-start 100)
(def small-start 1)

(defparam *big* big-start)
(defparam *small* small-start)

(defn guess-my-number []
  (quot (+ *big* *small*) 2))
             
(defn smaller []
  (setf *big* (dec (guess-my-number)))
  (guess-my-number))
             
(defn bigger []
  (setf *small* (inc (guess-my-number)))
  (guess-my-number))

(defn start-over []
  (defparam *big* big-start)
  (defparam *small* small-start)
  (guess-my-number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(deftest play-two-games
  (doseq [i (range 2)]
    (is (= (guess-my-number) 50))
    (is (= (smaller) 25))
    (is (= (smaller) 12))
    (is (= (bigger)  18))
    (is (= (bigger)  21))
    (is (= (bigger)  23))
    (is (= (start-over) 50))))
