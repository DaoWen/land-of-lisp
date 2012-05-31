(ns land-of-lisp.grand-theft-wumpus
  (:use [land-of-lisp.core]
        [clojure.set]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapters 8: Grand Theft Wumpus

;;;;;; city data ;;;;;;

(def node-count 30)
(def edge-count 45)
(def worm-count 3)
(def cop-odds 15)

;;;;;; game state ;;;;;;

(defparam *congestion-city-nodes* nil)
(defparam *congestion-city-edges* nil)
(defparam *visited-nodes* nil)

(defn random-node []
  (inc (rand-int node-count)))

;;;;;; city map functions ;;;;;;

(defn edge-pair [a b]
  (when-not (= a b)
    [{a b} {b a}]))

(defn make-edge-map []
  (let [base-map (into {} (for [n (range node-count)] [(inc n) #{}]))]
    (->> (repeatedly #(edge-pair (random-node) (random-node)))
         (remove nil?)
         (take edge-count)
         (apply concat)
         (apply merge-with conj base-map))))

(defn get-connected [node edge-map]
  (loop [connected #{node}]
    (let [connected' (apply union connected (map edge-map connected))]
      (if (= (count connected) (count connected'))
        connected
        (recur connected')))))

(defn find-islands [nodes edge-map]
  (loop [islands nil, work-list (set nodes)]
    (let [connected (get-connected (first work-list) edge-map)
          unconnected (difference work-list connected)
          islands' (cons connected islands)]
      (if (seq unconnected)
        (recur islands' unconnected)
        islands'))))

(defn connect-with-bridges [[connected & islands]]
  (if-let [island (first islands)]
    (concat (edge-pair (first connected) (first island))
            (connect-with-bridges islands))))

(defn connect-all-islands [nodes edge-map]
  (->> (find-islands nodes edge-map)
       (connect-with-bridges)
       (apply merge-with conj edge-map)))
