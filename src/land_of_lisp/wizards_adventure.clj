(ns land-of-lisp.wizards-adventure
  (:use [land-of-lisp.core]
        [clojure.string :only (trim-newline)]
        [clojure.test]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapters 5 & 6: Wizard's Adventure

(def nodes
  '{:living-room [You are in the living-room.
                  A wizard is snoring loudly on the couch.]
    :garden      [You are in a beautiful garden.
                  There is a well in front of you.]
    :attic       [You are in the attic.
                  There is a giant welding torch in the corner.]})

(def edges
  '{:living-room {:garden      {:dir west :via door} 
                  :attic       {:dir upstairs :via ladder}}
    :garden      {:living-room {:dir east :via door}}
    :attic       {:living-room {:dir downstairs :via ladder}}})

(defparam *object-locations*
  '{:whisky :living-room
    :bucket :living-room
    :chain  :garden
    :frog   :garden})

(defn describe-location [loc locations]
  (loc locations))

(defn describe-path [path]
  `[There is a ~(:via path) going ~(:dir path) from here.])

(defn describe-paths [location paths]
  (apply concat (map describe-path (vals (location paths)))))

(defn game-print [data]
  (println (apply str (interpose \space (map name data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(deftest game-print-test
  (letfn [(desc-loc [loc] (trim-newline (with-out-str (game-print (describe-location loc nodes)))))
          (desc-path [& path] (trim-newline (with-out-str (game-print (describe-path (get-in edges path))))))]
    (is (= (desc-loc :living-room)
           "You are in the living-room. A wizard is snoring loudly on the couch."))
    (is (= (desc-loc :garden)
           "You are in a beautiful garden. There is a well in front of you."))
    (is (= (desc-loc :attic)
           "You are in the attic. There is a giant welding torch in the corner."))
    (is (= (desc-loc :attic)
           "You are in the attic. There is a giant welding torch in the corner."))
    (is (= (desc-path :living-room :garden)
           "There is a door going west from here."))
    (is (= (desc-path :living-room :attic)
           "There is a ladder going upstairs from here."))))

(deftest describe-paths-test
  (let [str-map #(map name %)]
    (is (= (str-map (describe-paths :living-room edges))
           (str-map '[There is a door going west from here.
                      There is a ladder going upstairs from here.])))))
