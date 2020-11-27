(ns aoc.day01)

(defn- fuel [mass]
  (- (int (/ mass 3)) 2))

(defn p1 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (map fuel)
       (reduce +)))

(defn- fuel-fuel [mass]
  (let [init (fuel mass)]
    (loop [m init acc 0]
      (let [f (fuel m)]
        (cond
          (<= f 0) (+ init acc)
          :else    (recur f (+ acc f)))))))

(defn p2 [file]
  (->> file
       (slurp)
       (re-seq #"\d+")
       (map #(Integer/parseInt %))
       (map fuel-fuel)
       (reduce +)))
