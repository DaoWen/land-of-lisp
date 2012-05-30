(ns land-of-lisp.wizards-adventure
  (:require [clojure.string])
  (:use [land-of-lisp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapters 5 & 6: Wizard's Adventure

;;;;;; game world data ;;;;;;

(def nodes
  '{:living-room "You are in the living-room. A wizard is snoring loudly on the couch."
    :garden      "You are in a beautiful garden. There is a well in front of you."
    :attic       "You are in the attic. There is a giant welding torch in the corner."})

(def edges
  '{:living-room {:garden      {:dir west :via door} 
                  :attic       {:dir upstairs :via ladder}}
    :garden      {:living-room {:dir east :via door}}
    :attic       {:living-room {:dir downstairs :via ladder}}})

;;;;;; game state ;;;;;;

(defparam *object-locations*
  '{whiskey :living-room
    bucket  :living-room
    chain   :garden
    frog    :garden})

(defparam *location* :living-room)

;;;;;; game display functions ;;;;;;

(defn append-strs [strs]
  "Appends a collection of strings together, separating each string with a space.
   (Empty strings are ignored.)"
  (clojure.string/join " " (remove empty? strs)))

(defn format-msg [msg-str & args]
  "Formats a parameterized string with string, symbol or keyword arguments."
  (apply format msg-str (map name args)))

(defn describe-location [loc locations]
  (locations loc))

(defn describe-path [{portal :via direction :dir}]
  (format-msg "There is a %s going %s from here." portal direction))

(defn describe-paths [location paths]
  (append-strs (map describe-path (vals (paths location)))))

(defn objects-at [loc obj-locs]
  (set (filter #(= (obj-locs %) loc) (keys obj-locs))))

(defn describe-objects [loc obj-locs]
  (->> (objects-at loc obj-locs)
       (map #(format-msg "You see a %s on the floor." %))
       append-strs))

;;;;;; game commands ;;;;;;

(defn look []
  (append-strs [(describe-location *location* nodes)
                (describe-paths *location* edges)
                (describe-objects *location* *object-locations*)]))

(defn walk [direction]
  (if-let [next-location (->> (edges *location*)
                              (map (fn [[l {dir :dir}]] [dir l]))
                              (into {})
                              direction)]
    (do (setf *location* next-location) 
        (look))
    "You can't go that way."))

(defn pickup [object]
  (if ((objects-at *location* *object-locations*) object)
    (let [obj-locs' (assoc *object-locations* object :body)]
       (setf *object-locations* obj-locs')
       (format-msg "You are now carrying the %s" object))
    "You can't get that."))

(defn inventory []
  (let [format-obj-list #(clojure.string/join ", " (map name %))
        obj-list (format-obj-list (objects-at :body *object-locations*))]
    (if (seq obj-list)
      (format-msg "Items: %s" obj-list)
      "You have no items.")))

;;;;;; game repl ;;;;;;

(def legal-command? '#{look walk pickup inventory})

(def unary-command? '#{look inventory})

(def game-print println)

(defn game-read []
  (let [input (read-line)]
    (when-not (re-find #"[^\w ]" input)
      (read-string (str \( input \))))))

(defn game-eval [[cmd arg]]
  (cond (not (legal-command? cmd)) "Unknown command."
        (unary-command? cmd) ((resolve cmd))
        (nil? arg) "Missing argument to command."
        :else ((resolve cmd) arg)))

(defn game-repl []
  (game-print (look))
  (loop []
    (let [[cmd arg :as sexp] (game-read)]
      (when-not (= cmd 'quit)
        (game-print (game-eval sexp))
        (recur)))))
