(ns cr2.core
  (:gen-class)
  (:require [cr2.prompting :as p]))

(defmacro when-let* [bindings & body]
  (if (seq bindings)
    `(when-let [~(first bindings) ~(second bindings)]
       (when-let* ~(drop 2 bindings) ~@body))
    `(do ~@body)))

(def ^:private players 3) ;TODO config?
(def ^:private max-enemies 6)
(def ^:private level-power [11 14 18 23 32 35 41 44 49 53 62 68 71 74 82 84 103 119 131 141])
(def ^:private cr-power {0  1 1/8 5 1/4 10 1/2 16 1 22 2 28 3 37 4 48 5 60 6 65 7 70 8 85 9 85
                         10 95 11 105 12 115 13 120 14 125 15 130 16 140 17 150 18 160 19 165
                         20 180 21 200 22 225 23 250 24 275 25 300 26 325 27 350 28 375 29 400
                         30 425})
(def ^:private encounter-difficulties [{:tier "Mild" :multiplier 0.40 :cost 2}
                                       {:tier "Bruising" :multiplier 0.60 :cost 4}
                                       {:tier "Bloody" :multiplier 0.75 :cost 6}
                                       {:tier "Brutal" :multiplier 0.90 :cost 8}
                                       {:tier "Oppressive" :multiplier 1.00 :cost 10}
                                       {:tier "Overwhelming" :multiplier 1.10 :cost 13}
                                       {:tier "Crushing" :multiplier 1.30 :cost 17}
                                       {:tier "Devastating" :multiplier 1.60 :cost 25}
                                       {:tier "Impossible" :multiplier 2.25 :cost 50}
                                       {:tier "Custom" :multiplier :custom :cost :custom}])

(defn- combinations-without-replacement [coll r]
  "Uses algorithm from https://docs.python.org/2/library/itertools.html#itertools.combinations_with_replacement"
  (let [pool (vec coll)
        n (count pool)]
    (loop [indices (vec (repeat r 0))
           result [(mapv pool indices)]]
      (if-let [i (reduce (fn [_ i]
                           (when-not (= (get indices i) (dec n))
                             (reduced i)))
                         nil (range (dec r) -1 -1))]
        (let [indices (-> (subvec indices 0 i)
                          (into (repeat (- r i) (inc (get indices i)))))]
          (recur indices
                 (conj result (mapv pool indices))))
        result))))

(defn- unordered-legal-selections [crs n lower upper]
  (sequence (comp
              (map (fn [crs] {:crs   crs
                              :total (transduce (map cr-power) + crs)}))
              (filter #(<= lower (:total %) upper)))
            (combinations-without-replacement crs n)))

(defn- select-multiplier []
  (when-let [{:keys [multiplier]} (p/>>item "What is the target difficulty of this encounter?" encounter-difficulties :sorted? false)]
    (if (= :custom multiplier)
      (some-> 1 #_(p/>>input "What is the difficulty multiplier?") parse-double)
      multiplier)))

;TODO change to customised max amount of monsters, then derive min CR based on that
;https://www.gmbinder.com/share/-N4m46K77hpMVnh7upYa
(defn cr2-encounter []
  (when-let* [level (some-> (p/>>input "What level are the players?") parse-long)
              min-cr (p/>>item "What is the minimum CR of monsters in this encounter?"
                               (keys cr-power))
              multiplier (select-multiplier)]
    (let [player-power (->> (dec level)
                            (nth level-power)
                            (* players))
          target-monster-power (* multiplier player-power)
          target-monster-power-lower (* 0.95 target-monster-power)
          target-monster-power-upper (* 1.05 target-monster-power)
          crs (keep (fn [[cr power]]
                      (when (and (>= cr min-cr) (<= power player-power))
                        cr))
                    cr-power)
          cr-options (reduce (fn [acc n]
                               (if-let [legal-cr-combos (-> (unordered-legal-selections
                                                              crs n target-monster-power-lower target-monster-power-upper)
                                                            not-empty)]
                                 (assoc acc n legal-cr-combos)
                                 (if (empty? acc)
                                   acc
                                   (reduced acc))))
                             {}
                             (range 1 (inc max-enemies)))]
      {:target  target-monster-power
       :options cr-options})))

(defn -main [& _]
  (cr2-encounter))