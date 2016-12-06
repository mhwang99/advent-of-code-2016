(defn getf
  [m e]
  (loop [i 0 e e m m]
    (if (empty? e) m
      (let [f (first e)]
        (if (get-in m [i f])
          (recur (inc i) (rest e) (update-in m [i f] inc))
          (recur (inc i) (rest e) (assoc-in m [i f] 1)))))))

(defn getmostf
  [m]
  (map #(->> %
             last
             (sort-by last)
             last
             first)
       (sort-by first m)))

(defn getleastf
  [m]
  (map #(->> %
             last
             (sort-by last)
             first
             first)
       (sort-by first m)))


(defn day6
  [f l]
  (apply str (f (reduce #(getf %1 %2) {} l))))

(day6 getmostf (clojure.string/split (slurp "day6.txt") #"\s+"))

