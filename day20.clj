(defn parse
  [l]
  (let [l (sort-by first l)
        [e l] (reduce (fn [[[a b] l] [c d]]
                        (if (<= (dec c) b) [[a (max b d)] l]
                          [[c d] (conj l [a b])]))
                      [(first l) []]
                      (rest l))]
    (conj l e)))

(defn proc
  [l]
  (let [l (parse l)
        _ (println "part 1" (inc (last (first l))))
        s (reduce #(- %1 1 (- (last %2) (first %2))) 4294967296 l)]
    (println "part 2" s)))

(proc (partition 2 (map #(BigInteger. %) (str/split (slurp "day20.txt") #"[\n-]"))))

