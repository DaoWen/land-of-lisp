(ns land-of-lisp.evolution
  (:use [land-of-lisp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapters 10: Evolution

;;;;;; simulation configuration ;;;;;;

(def env-width 100)
(def env-height 30)
(def jungle-box [45 10 10 10])
(def plant-energy 80)

;;;;;; simulation state ;;;;;;

(def plants (make-array Boolean/TYPE env-width env-height))

;;;;;; growing plants ;;;;;;

(defn random-plant [left top width height]
   (let [x (+ left (rand-int width))
         y (+ top  (rand-int height))]
     (aset-boolean plants x y true)))

(defn add-plants []
   (apply random-plant jungle-box)
   (random-plant 0 0 env-width env-height))
