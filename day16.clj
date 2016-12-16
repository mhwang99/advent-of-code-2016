
(defn genstr
  [s n]
  (loop [s (vec s)]
    (if (>= (count s) n) (take n s)
      (recur (concat s [\0] (mapv #(case % \0 \1 \0) (reverse s)))))))

(defn chks
  [s]
  (loop [s s]
    (let [l (partition 2 s)
          s (reduce #(if (= (first %2) (last %2)) (conj %1 \1)
                 (conj %1 \0)) [] l)]
      (if (odd? (count s)) s
        (recur s)))))

(defn proc
  [s n]
  (chks (genstr s n)))


;part 1
(apply str (proc "01110110101001000" 272))

;part 2
(apply str (proc "01110110101001000" 35651584))
