(defn wall?
  [s [x y :as p] d]
  (cond
    (get s p) true
    (or (< x 0)
        (< y 0)
        (> x 100)
        (> y 100)) true
    :else (let [i (+ (* x x) (* 3 x) (* 2 x y) y (* y y) d)
                b (Integer/toBinaryString i)
                c (count (re-seq #"1" b))]
            (odd? c))))

(defn find-min
  [g [x y :as p] d n s]
  (if (= g p) n
    (if (wall? s p d) nil
      (let [s (conj s p)
            t1 (find-min g [(inc x) y] d (inc n) s)
            t2 (find-min g [(dec x) y] d (inc n) s)
            t3 (find-min g [x (inc y)] d (inc n) s)
            t4 (find-min g [x (dec y)] d (inc n) s)]
        (reduce #(cond
                   (nil? %1) %2
                   (nil? %2) %1
                   (< %1 %2) %1
                   :else %2) nil [t1 t2 t3 t4])))))

(defn find-path
  [g [x y :as p] d n s]
  (if (wall? s p d) s
    (let [s (conj s p)]
      (if (= n g) s
        (let [s1 (find-path g [(inc x) y] d (inc n) s)
              s2 (find-path g [(dec x) y] d (inc n) s)
              s3 (find-path g [x (inc y)] d (inc n) s)
              s4 (find-path g [x (dec y)] d (inc n) s)]
          (reduce into [s1 s2 s3 s4]))))))

;;part 1
(find-min [31 39] [1 1] 1350 0 #{})
;;part 2
(count (find-path 50 [1 1] 1350 0 #{}))

