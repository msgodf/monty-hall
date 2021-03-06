(ns monty-hall.core
     (:require [clojure.set :as set]))

(defn generate-world
  [world-number]
  (get [[:car :goat :goat]
        [:goat :car :goat]
        [:goat :goat :car]]
       world-number))

(defn monty-hall
  [world choice]
  (case world
    [:car :goat :goat] (case choice
                         0 (rand-nth [[:_ :goat :_]
                                      [:_ :_ :goat]])
                         1 [:_ :_ :goat]
                         2 [:_ :goat :_])
    [:goat :car :goat] (case choice
                         0 [:_ :_ :goat]
                         1 (rand-nth [[:goat :_ :_]
                                      [:_ :_ :goat]])
                         2 [:goat :_ :_])
    [:goat :goat :car] (case choice
                         0 [:_ :goat :_]
                         1 [:goat :_ :_]
                         2 (rand-nth [[:goat :_ :_]
                                      [:_ :goat :_]]))))

(defn stick
  [world door-choice _]
  (get world door-choice))

(defn switch
  [world door-choice knowledge]
  (let [goat-index (.indexOf knowledge :goat)]
    (get world
         (first
          (disj (set (range (count world)))
                goat-index
                door-choice)))))

(defn run
  [strategy]
  (let [world (generate-world (rand-int 3))
        door-choice (rand-int 3)]
    (strategy world
              door-choice
              (monty-hall world door-choice))))

(defn simulate
  [strategy n]
  (->> (range n)
       (map (fn [_] (run strategy)))
       (group-by identity)
       (map (fn [[k v]] [k (count v)]))
       (into {})))

#_(simulate stick 1000);;=>{:goat 672, :car 328}
#_(simulate switch 1000);;=>{:car 669, :goat 331}
