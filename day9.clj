
(defn fx
  [l]
  (let [[t n x] (re-find #"\((\d+)x(\d+)\)" (apply str l))
        n (Integer. n)
        x (Integer. x)
        s (->> l
               (drop (count t))
               (take n)
               flatten
               vec)]
    [(+ (count t) n) x n s]))

(defn fp1 [l]
  (loop [l (vec l) r 0]
    (if (empty? l) r
      (if (= (first l) \()
        (let [[t x i _] (fx l)]
          (recur (drop t l) (+ r (* x i))))
        (recur (rest l) (inc r))))))

(defn fp2 [l]
  (loop [l l r 0]
    (if (empty? l) r
      (if (= \( (first l))
        (let [[t x _ s] (fx l)]
          (recur (drop t l) (+ r (* x (fi s)))))
        (recur (rest l) (inc r))))))

(fp1 (vec (drop-last 1 (slurp "day9.txt"))))
(fp2 (vec (drop-last 1 (slurp "day9.txt"))))
