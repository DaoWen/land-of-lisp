(ns land-of-lisp.grand-theft-wumpus
  (:use [land-of-lisp.core]
        [clojure.set]
        [vijual :only (draw-graph)]))

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
(defparam *player-pos* nil)

;;;;;; city map's edge building functions ;;;;;;

(defn random-node []
  (inc (rand-int node-count)))

(defn edge-pair [a b]
  (when-not (= a b)
      #{{a b} {b a}}))

(defn make-edge-map []
  (let [base-map (into {} (for [n (range node-count)] [(inc n) #{}]))]
    (->> (repeatedly #(edge-pair (random-node) (random-node)))
         (remove nil?)
         (distinct)
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

(defn attributed-edge-map [edge-map]
  (into {} (for [[start dests] edge-map]
             [start (into {} (map #(vector % {}) dests))])))

(defn add-cops [edge-amap edges-with-cops]
  (loop [edges edge-amap, cops (vec edges-with-cops)]
    (if-let [cop (peek cops)]
      (let [[a b] cop
            edges'  (assoc-in edges  [a b :cops] true)
            edges'' (assoc-in edges' [b a :cops] true)]
        (recur edges'' (pop cops)))
      edges)))

(defn make-city-edges []
  (let [nodes (range 1 (inc node-count))
        edge-map (connect-all-islands nodes (make-edge-map))
        cops (for [[start dests] edge-map
                   dest dests
                   :when (< start dest)
                   :when (zero? (rand-int cop-odds))]
               [start dest])]
    (add-cops (attributed-edge-map edge-map) cops)))

;;;;;; city map's node building functions ;;;;;;

(defn neighbors [x edge-amap]
  (keys (edge-amap x)))

(defn within-one? [a b edge-amap]
  (get-in edge-amap [a b]))

(defn within-two? [a b edge-amap]
  (or (within-one? a b edge-amap)
      (some #(within-one? % b edge-amap) (neighbors a edge-amap))))

(defn make-city-nodes [edge-amap]
  (->> (let [wumpus (random-node)
             glow-worms (repeatedly worm-count random-node)
             glow-worms? (set glow-worms)
             cops? #(:cops (get-in edge-amap [% %2]))]
         (for [n (range 1 (inc node-count))]
           (let [w (cond (= n wumpus)           {:wumpus true}
                         (within-two? n wumpus edge-amap) {:blood true})
                 g (cond (glow-worms? n)        {:glow-worms true}
                         (some #(within-one? n % edge-amap) glow-worms) {:lights true})
                 c (when (some #(cops? n %) (neighbors n edge-amap)) {:sirens true})
                 attrs (merge {} w g c)]
             [n attrs])))
       (into {})))

;;;;;; city map drawing functions ;;;;;;

(defn node-label [n node-amap]
  (->> n node-amap keys (map name) (cons n)
       (interpose \space)
       (apply str)))

(defn draw-city-graph [city-nodes city-edges]
  (let [nodes (keys city-nodes)
        edges (distinct (for [[a bs] city-edges
                              [b vs] bs]
                          [(min a b) (max a b)]))
        break (fn [[a b]] (let [c (gensym 'cops)] [[a c] [c b]]))
        edges (mapcat #(if (:cops (get-in city-edges %)) (break %) [%]) edges)
        labels (into {} (map (fn [n] [n (node-label n city-nodes)]) nodes))
        cop-nodes (filter symbol? (map first edges))
        labels (into labels (map (fn [n] [n "cops"]) cop-nodes))]
    (draw-graph edges labels)))

(defn draw-city []
  (draw-city-graph *congestion-city-nodes* *congestion-city-edges*))

(defn node-visited? [n]
  (*visited-nodes* n))

(defn known-city-nodes []
  (let [fringe (mapcat #(neighbors % *congestion-city-edges*) *visited-nodes*)
        node-visible? (into *visited-nodes* fringe)]
    (into {}
          (for [[n attrs] *congestion-city-nodes*
                :when (node-visible? n)]
            (cond (= *player-pos* n) [n (merge attrs '{* *})]
                  (node-visited? n)  [n attrs]
                  :else              [n '{? ?}])))))

(defn known-city-edges []
  (->> *congestion-city-edges*
       (filter (fn [[a]] (node-visited? a)))
       (into {})))

(defn draw-known-city []
  (draw-city-graph (known-city-nodes) (known-city-edges)))

;;;;;; game start functions ;;;;;;

(defn find-empty-node [nodes]
  (->> (repeatedly random-node)
       (remove #(seq (nodes %)))
       first))

(defn new-game []
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node *congestion-city-nodes*))
  (setf *visited-nodes* #{*player-pos*})
  (draw-known-city))

;;;;;; game control functions ;;;;;;

(defn handle-new-place [edge pos charging]
  (let [node (*congestion-city-nodes* pos)
        has-worm (and (:glow-worms node)
                       (not (node-visited? pos)))]
    (setf *visited-nodes* (conj *visited-nodes* pos))
    (setf *player-pos* pos)
    (draw-known-city)
    (cond (:cops edge) (println "You ran into the cops. Game Over.")
          (:wumpus node) (println (if charging
                                    "You got the Wumpus!"
                                    "You got capped by the Wumpus! Game Over."))
          charging (println "You wasted your last bullet. Game Over.")
          has-worm (let [pos' (random-node)]
                     (println (format "You ran into a Glow Worm Gang! You're now at %d." pos'))
                     (handle-new-place nil pos' nil)))))

(defn handle-direction [pos charging]
  (let [edge (get-in *congestion-city-edges* [*player-pos* pos])]
    (if edge
      (handle-new-place edge pos charging)
      (println "You can't go that way!"))))

(defn walk [pos]
  (handle-direction pos nil))

(defn charge [pos]
  (handle-direction pos true))
