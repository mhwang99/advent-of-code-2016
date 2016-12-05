(defn get-c
  [s]
  (let [sm (reduce (fn [m k]
            (if (get m k)
             (update m k inc)
             (assoc m k 1))) {} s)
        sm (sort (fn [a b]
                  (let [cp (- (last b) (last a))]
                    (if (= 0 cp)
                      (compare (first a) (first b))
                      cp))) sm)]
    (apply str (keys (take 5 sm)))))


(defn decr
  [s n]
  (apply str (for [c s]
    (-> c
        int
        (- (int \a))
        (+ n)
        (mod 26)
        (+ (int \a))
        char))))

(defn chk
  [[en c _]]
  (let [s (apply str (re-seq #"[a-z]+" en))
        n (Integer. (re-find #"\d+" en))]
    (if (= (get-c s) c)
      (let [ds (decr s n)]
        (when (re-find #"north" ds)
          (println ds n))
        n)
      0)))

(defn day4 [l]
  (reduce #(+ %1 (chk %2)) 0 l))

(day4 (partition 3 (clojure.string/split (slurp "day4.txt") #"[\[\]\s]")))

