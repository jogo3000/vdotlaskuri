(ns vdotlaskuri
  (:import (java.lang Math)))

(defn %VOmax
  "Returns the percentage of VOmax a runner can sustain for the given duration"
  [seconds]
  (let [mins (/ seconds 60)]
    (+ 0.8
       (* 0.1894393
          (Math/exp (* -0.012778 mins)))
       (* 0.2989558
          (Math/exp (* -0.1932605 mins))))))

(defn VO2
  "Returns the oxygen consumption of a runner when running in a given speed"
  [v]
  (+ -4.60
     (* 0.182258 v)
     (* 0.000104 (Math/pow v 2))))

(defn ->velocity
  "Gilbert's formulas use meters / minutes as units. Calculates the speed required to cover a distance in the given time
  using these units"
  [s t]
  (/ s (/ t 60)))

(defn pace->velocity
  "Pace is seconds / 1000 m so..."
  [pace]
  (int (/ 1000 (/ pace 60))))

(defn velocity->pace
  "velocity is m / min so...
  v = m / min and we want the pace per 1000 m
  v = 1000 m / min -> min = 1000 m / v"
  [velocity]
  (int (* 60 (/ 1000 velocity))))

(defn VO2->velocity
  "Calculates theoretical velocity when runner consumes given amount of oxygen"
  [vo2]
  (+ 29.54
     (* 5.000663 vo2)
     (* -0.007546 (Math/pow vo2 2))))

(defn VDot
  "Calculates the pseudo VO2max, VDot based on a race distance and duration"
  [race-distance race-duration]
  (/ (VO2 (->velocity race-distance race-duration))
     (%VOmax race-duration)))

(defn predict
  "Finds the theoretical best time for a race distance given vdot is known. Doesn't support efforts that take more than three hours."
  [vdot race-distance]
  (->> (range 1 (* 4 60 60))
       (map (juxt identity #(VDot race-distance %)))
       (drop-while (fn [[t v]] (< vdot v)))
       (ffirst)))

(defn seconds->min-seconds [s]
  ((juxt quot rem) s 60))

(defn seconds->str [s]
  ((fn [[min seconds]] (str min "'" (when (< seconds 10) "0") seconds)) (seconds->min-seconds s)))

(defn race-predictions
  "Predicts performance in some common racing distances given an earlier race performance"
  [race-distance race-duration]
  (let [vdot (VDot race-distance race-duration)
        common-race-distances [800 1500 3000 5000 10000 21095]]
    (for [d common-race-distances]
      [d (seconds->str (predict vdot d))])))

(defn effort [multiplier vdot]
  (-> (* multiplier vdot)
      (VO2->velocity)
      (velocity->pace)
      (seconds->str)))

(defn training-paces
  "Prescribes suitable training paces given an earlier race performance"
  [race-distance race-duration]
  (let [vdot (VDot race-distance race-duration)
        effort-levels [[:easy [0.59 0.74]]
                       [:marathon [0.75 0.84]]
                       [:threshold [0.83 0.88]]
                       [:interval [0.95 1.00]]
                       [:maximal [1.00 1.05]]]]
    (for [[level [low high]] effort-levels]
      [level (mapv #(effort % vdot) [low high])])))

(defn safe-parseint [s]
  (try
    (Integer/parseInt s)
    (catch Exception e 0)))

(defn parse-time [time]
  (as-> (clojure.string/split time #"[^\d]") parts
    (map safe-parseint parts)
    (cond (= 3 (count parts)) (+ (* 60 60 (first parts))
                                 (* 60 (second parts))
                                 (last parts))
          (= 2 (count parts)) (+ (* 60 (first parts))
                                 (last parts))
          (= 1 (count parts)) (first parts)
          :else 0)))

(defn predict-and-prescribe [race-distance race-duration]
  [(training-paces race-distance race-duration)
   (race-predictions race-distance race-duration)])


(require '[clojure.spec.alpha :as s])
(require '[clojure.spec.gen.alpha :as sgen])
(require '[clojure.spec.test.alpha :as stest])

(s/fdef parse-time
  :args (s/cat :time string?)
  :ret int?)
