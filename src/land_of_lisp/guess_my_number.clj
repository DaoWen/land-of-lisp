(ns land-of-lisp.guess-my-number
  (:use [land-of-lisp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapter 2: Guess My Number

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
