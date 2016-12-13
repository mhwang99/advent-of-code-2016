(defn wall?
  [s [x y :as p] d]
  (cond
    (get s p) true
    (or (< x 0) (< y 0)) true
    :else (let [i (+ (* x x) (* 3 x) (* 2 x y) y (* y y) d)
                b (Integer/toBinaryString i)
                c (count (re-seq #"1" b))]
            (odd? c))))

(defn find-min
  [g p d mn]
  (loop [s #{} l [p] n 0]
    (if (or (= (inc mn) n) (some #{g} l)) {:step n :reach (count s)}
      (let [[s nl]
            (reduce (fn [[s nl] [x y :as p]]
                      (if (wall? s p d) [s nl]
                        (let [s (conj s p)
                              cl [[(inc x) y] [(dec x) y] [x (inc y)] [x (dec y)]]]
                          [s (reduce conj nl cl)]))) 
                    [s l] l)]
        (recur s nl (inc n))))))

;part 1
(find-min [31 39] [1 1] 1350 -2)
;part 2
(find-min [-1 -1] [1 1] 1350 50)

