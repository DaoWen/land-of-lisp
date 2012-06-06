(ns land-of-lisp.orc-battle
  (:use [land-of-lisp.core]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chapters 9: Orc Battle

;;;;;; game data ;;;;;;

(def monsters-count 12)

;;;;;; game state ;;;;;;

(defparam *player-health* nil)
(defparam *player-agility* nil)
(defparam *player-strength* nil)
(defparam *monsters* nil)

;;;;;; monster types ;;;;;;

(defrecord Monster [health])

(defrecord Orc [health club-level])

(defrecord Hydra [health])

(defrecord SlimeMold [health sliminess])

(defrecord Brigand [health])

(defn randval [n] (inc (rand-int (max 1 n))))

(defn rand-health [] (randval 10))

(def monster-builders
  [#(Orc.       (rand-health) (randval 8))
   #(Hydra.     (rand-health))
   #(SlimeMold. (rand-health) (randval 5))
   #(Brigand.   (rand-health))])

;;;;;; game monsters functions ;;;;;;

(defn monster-type [m]
  (->> (type m)
       .getName
       (re-seq #"[^./]+")
       last))

(defn update-monster-health [m op delta]
  (let [index (.indexOf *monsters* m)
        health' (op (:health m) delta)
        m' (assoc m :health health')
        monsters' (assoc *monsters* index m')]
    (setf *monsters* monsters')
    m'))

(defn monster-dead? [m]
  (not (pos? (:health m))))

(defn monsters-dead? []
  (every? monster-dead? *monsters*))

(defn random-monster []
  (let [m (rand-nth *monsters*)]
    (if (monster-dead? m) (recur) m)))

(defmulti monster-hit (fn [m x] (type m)))

(defmethod monster-hit :default [m x]
  (update-monster-health m - x)
  (println
    (if (monster-dead? m)
      (format "You killed the %s!"
              (monster-type m))
      (format "You hit the %s knocking off %d health points!"
              (monster-type m) x))))

(defmethod monster-hit Hydra [m x]
  (println
    (if (monster-dead? (update-monster-health m - x))
      "The corpse of the fully decapitated and decapacitated hydra falls to the floor!"
      (format "You lop off %d of the hydra's heads!" x))))

(defmulti monster-attack type)

(defmethod monster-attack Orc [m]
  (let [x (randval (:club-level m))]
    (println
      (format
        "An orc swings his club at you and knocks off %d of your health points." x))
    (setf *player-health* (- *player-health* x))))

(defmethod monster-attack Hydra [m]
  (let [x (randval (quot (:health m) 2))]
    (println
      (format "A hydra attacks you with of its heads! It also grows back one more head!"
              x))
    (update-monster-health m + 1)
    (setf *player-health* (- *player-health* x))))

(defmethod monster-attack SlimeMold [m]
  (let [x (randval (:sliminess m))]
    (println
      (format "A slime mold wraps around your legs and decreases your agility by %d!" x))
    (setf *player-agility* (- *player-agility* x))
    (when (zero? (rand-int 2))
      (println "It also squirts in your face, taking away a health point!")
      (setf *player-health* (- *player-health* 1)))))

(defmethod monster-attack Brigand [m]
  (let [x (max *player-health* *player-agility* *player-strength*)]
    (condp = x
      *player-health*
      (do (println "A brigand hits you with his slingshot, taking off 2 health points!")
          (setf *player-health* (- *player-health* 2)))
      *player-agility*
      (do (println "A brigand catches your leg with his whip, taking off 2 agility points!")
          (setf *player-agility* (- *player-agility* 2)))
      *player-strength*
      (do (println "A brigand cuts your arm with his whip, taking off 2 strength points!")
          (setf *player-strength* (- *player-strength* 2))))))

(defmulti monster-desc type)

(defmethod monster-desc :default [m]
  (format "A fierce %s!" (monster-type m)))

(defmethod monster-desc Hydra [m]
  (format "A malicious hydra with %d heads."
          (:health m)))

(defmethod monster-desc SlimeMold [m]
  (format "A slime mold with a sliminess of %s."
          (:sliminess m)))

(defn show-monsters []
  (println)
  (println "Your foes:")
  (doseq [[m x] (map list *monsters* (rest (range)))]
    (println
      (format "%n    %2d. %s"
              x
              (if (monster-dead? m)
                "**dead**"
                (format "(Health=%d) %s"
                        (:health m) (monster-desc m)))))))

;;;;;; game hero functions ;;;;;;

(defn player-dead? []
  (not (pos? *player-health*)))

(defn show-player []
  (println
    (format
      "%nYou are a valiant knight with a health of %d, an agility of %d, and a strength of %d."
      *player-health* *player-agility* *player-strength*)))

;;;;;; game main loop ;;;;;;

(defn pick-monster []
  (println)
  (print "Monster #: ")
  (let [x (read)]
    (if-not (and (integer? x) (contains? *monsters* (dec x)))
      (do (println "That's not a valid monster number.")
          (pick-monster))
      (let [m (*monsters* (dec x))]
        (if (monster-dead? m)
          (do (println "That monster is alread dead.")
              (pick-monster))
          m)))))

(defn player-attack []
  (println)
  (println "Attack style: [s]tab [d]ouble swing [r]oundhouse:")
  (case (read)
    s (monster-hit (pick-monster)
                    (+ 2 (randval (quot *player-strength* 2))))
    d (let [x (randval (quot *player-strength* 6))]
         (println (format "Your double swing has a strength of %d.%n" x))
         (monster-hit (pick-monster) x)
         (when-not (monsters-dead?)
           (monster-hit (pick-monster) x)))
    (dotimes [x (inc (randval (quot *player-strength* 3)))]
      (when-not (monsters-dead?)
        (monster-hit (random-monster) 1)))))

(defn game-loop []
  (when-not (or (player-dead?) (monsters-dead?))
    (show-player)
    (dotimes [k (inc (quot (max 0 *player-agility*) 15))]
      (when-not (monsters-dead?)
        (show-monsters)
        (player-attack)))
    (println)
    (doseq [m *monsters*
          :when (not (monster-dead? m))]
      (monster-attack m))
    (recur)))

;;;;;; game start-up ;;;;;;

(defn init-monsters []
  (->> #((rand-nth monster-builders))
       (repeatedly monsters-count)
       (vec)
       (setf *monsters*)))

(defn init-player []
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defn orc-battle []
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead?)
    (println "You have been killed. Game Over."))
  (when (monsters-dead?)
    (println "Congratulations! You have vanquished all of your foes.")))
