
(def src "^..^^.^^^..^^.^...^^^^^....^.^..^^^.^.^.^^...^.^.^.^.^^.....^.^^.^.^.^.^.^.^^..^^^^^...^.....^....^.")

(defn get-nl
  [s]
  (let [s (concat [\.] s [\.])]
    (map #(if (= %1 %2) \. \^) s (drop 2 s))))

(defn proc
  [s g]
  (loop [r 0 n 1 s s]
    (let [r (+ r (reduce #(case %2 \. (inc %1) %1) 0 s))]
      (if (= g n) r
        (recur r (inc n) (get-nl s))))))

;part1
(proc src 40)
;part2
(proc src 400000)

