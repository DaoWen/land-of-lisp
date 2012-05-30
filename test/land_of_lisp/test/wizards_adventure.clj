(ns land-of-lisp.test.wizards-adventure
  (:use [land-of-lisp.wizards-adventure]
        [land-of-lisp.core]
        [clojure.test]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapters 5 & 6: Wizard's Adventure

(deftest game-print-test
  (letfn [(desc-loc [loc] (with-out-str (game-print (describe-location loc nodes))))
          (desc-path [& path] (with-out-str (game-print (describe-path (get-in edges path)))))]
    (is (= (desc-loc :living-room)
           "You are in the living-room. A wizard is snoring loudly on the couch.\n"))
    (is (= (desc-loc :garden)
           "You are in a beautiful garden. There is a well in front of you.\n"))
    (is (= (desc-loc :attic)
           "You are in the attic. There is a giant welding torch in the corner.\n"))
    (is (= (desc-loc :attic)
           "You are in the attic. There is a giant welding torch in the corner.\n"))
    (is (= (desc-path :living-room :garden)
           "There is a door going west from here.\n"))
    (is (= (desc-path :living-room :attic)
           "There is a ladder going upstairs from here.\n"))))

(deftest describe-paths-test
  (is (= (describe-paths :living-room edges)
         "There is a door going west from here. There is a ladder going upstairs from here."))
  (is (= (describe-paths :garden edges)
         "There is a door going east from here.")))

(deftest describe-objects-test
  (is (= (describe-objects :attic *object-locations*) ""))
  (is (= (describe-objects :living-room *object-locations*)
         "You see a whiskey on the floor. You see a bucket on the floor."))
  (is (= (describe-objects :garden *object-locations*)
         "You see a frog on the floor. You see a chain on the floor.")))

(deftest inventory-pickup-test
  (let [inv= #(is (= (inventory) %))
        empty "You have no items."
        orig-obj-locs *object-locations*]
    (inv= empty)
    (is (= (pickup 'food) "You can't get that."))
    (inv= empty)
    (pickup 'bucket)
    (inv= "Items: bucket")
    (pickup 'whiskey)
    (inv= "Items: whiskey, bucket")
    (setf *object-locations* orig-obj-locs)
    (inv= empty)))

(deftest walk-test
  (let [orig-loc *location*]
    (is (= *location* :living-room))
    (walk 'west)
    (is (= *location* :garden))
    (walk 'west)
    (is (= *location* :garden))
    (walk 'east)
    (is (= *location* :living-room))
    (walk 'east)
    (is (= *location* :living-room))
    (walk 'north)
    (is (= *location* :living-room))
    (walk 'upstairs)
    (is (= *location* :attic))
    (setf *location* orig-loc)))
