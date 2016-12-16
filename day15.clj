(defn proc
  [has start]
  (loop [n 1]
    (let [p (->> start
                (map + (range n (+ n 999999)))
                (map #(mod %2 %1) has))]
      (if (apply = 0 p) (dec n)
        (recur (inc n))))))

;part1
(proc
[17 7 19 5 3 13]
[ 1 0  2 0 0 5])

;part2
(proc
[17 7 19 5 3 13 11]
[ 1 0  2 0 0  5  0])
