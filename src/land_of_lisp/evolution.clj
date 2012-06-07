(ns land-of-lisp.evolution
  (:use [land-of-lisp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapters 10: Evolution

;;;;;; simulation configuration ;;;;;;

(def env-width 100)
(def env-height 30)
(def jungle-box [45 10 10 10])
(def plant-energy 80)
(def reproduction-energy 200)

;;;;;; simulation state ;;;;;;

(defparam *plants* (make-array Boolean/TYPE env-width env-height))
(defparam *animals* [{:x (quot env-width 2)
                      :y (quot env-height 2)
                      :energy 1000
                      :dir 0
                      :genes (vec (repeatedly 8 #(inc (rand-int 10))))}])

;;;;;; growing plants ;;;;;;

(defn random-plant [left top width height]
   (let [x (+ left (rand-int width))
         y (+ top  (rand-int height))]
     (aset-boolean *plants* x y true)))

(defn add-plants []
   (apply random-plant jungle-box)
   (random-plant 0 0 env-width env-height))

;;;;;; animal actions ;;;;;;

(def ^{:doc "[dx dy] pairs for directions 0 thru 7"}
  directions [[-1 -1] [0 -1] [1 -1] [1 0] [1 1] [0 1] [-1 1] [-1 0]])

(defn move [animal]
  (let [[dx dy] (directions (:dir animal))
        x' (mod (+ dx (:x animal)) env-width)
        y' (mod (+ dy (:y animal)) env-height)
        energy' (dec (:energy animal))]
    (assoc animal :x x' :y y' :energy energy')))

(defn turn [animal]
  (letfn [(angle [genes x]
            (let [x' (- x (first genes))]
              (if (neg? x') 0 (inc (angle (rest genes) x')))))]
    (let [x (rand-int (apply + (:genes animal)))]
      (assoc animal :dir (mod (+ (:dir animal) (angle (:genes animal) x)) 8)))))

(defn eat [animal]
  (let [x (:x animal)
        y (:y animal)]
    (if (aget *plants* x y)
      (do (aset-boolean *plants* x y false)
          (update-in animal [:energy] #(+ % plant-energy)))
      animal)))

(defn reproduce [animal]
  (if (>= (:energy animal) reproduction-energy)
    (let [animal' (update-in animal [:energy] #(quot % 2))
          mutation-index (rand-int 8)
          child-animal (update-in animal' [:genes mutation-index]
                                  #(max 1 (+ % (rand-int 3) -1)))]
      [animal' child-animal])
    [animal]))

;;;;;; simulation controls ;;;;;;

(defn update-world []
  (let [live-animals (filter #(pos? (:energy %)) *animals*)]
    (setf *animals* (doall (mapcat #(-> % turn move eat reproduce) live-animals))))
  (add-plants))

(defn draw-world []
  (doseq [y (range 0 env-height)]
    (printf "%n|")
    (doseq [x (range 0 env-width)]
      (print (cond
               (some #(and (== (:x %) x) (== (:y %) y)) *animals*) \M
               (aget *plants* x y) \*
               :else \space)))
    (print "|"))
  (println))

(defn evolution []
  (draw-world)
  (let [input (read-line)]
    (when-not (= input "quit")
      (if-let [int-string (re-find #"\d+" input)]
        (dotimes [i (Integer/valueOf int-string)]
          (update-world)
          (when (zero? (mod i 1000)) (print \.) (flush)))
        (update-world))
      (recur))))
