
(defn safe?
  [l]
  (let [h (map #(vector (first %) :g)
               (filter #(= (second %) :h) l))
        g (set (filter #(= (second %) :g) l))]
    (or (empty? g) (every? g h))))

(defn get-ll
  [ll mv from to]
  (let [lfrom (set (remove mv (get ll from)))
        lto (into (get ll to) mv)]
    (-> ll
        (assoc from lfrom)
        (assoc to lto))))

(defn nextlell [[e ll]]
  (let [l (get ll e)
        mv (->> (reduce #(into %1 (map conj %1 (repeat %2))) #{#{}} l)
                (filter #(case (count %) (1 2) true false))
                (filter #(safe? (remove % l))))
        to-dec (if (or (= e 0)
                       (= 0 (reduce
                              #(+ %1 (count (get ll %2)))
                              0 (range e)))) []
                 (filter #(safe? (into (get ll (dec e)) %)) mv))
        to-inc (if (= e 3) []
                 (filter #(safe? (into (get ll (inc e)) %)) mv))]
    (concat
      (map #(vector (dec e) (get-ll ll % e (dec e))) to-dec)
      (map #(vector (inc e) (get-ll ll % e (inc e))) to-inc))))

(defn done? [[e ll]]
  (and (= e 3)
       (= 0 (reduce
              #(+ %1 (count (get ll %2)))
              0 (range 3)))))

(defn make-history
  [h lell]
  (reduce #(if (get %1 %2) %1
             (assoc %1 %2 {:ing true})) h lell))

(defn find-min
  [ll e]
  (loop [h (make-history {} [[e ll]]) n 0]
    (let [lell (map first (filter #(:ing (second %)) h))
          h (into {} (map #(vector (first %) {:ing false}) h))
          done (filter done? lell)]
      (if (not-empty done) n
        (recur (reduce #(make-history %1 (nextlell %2))
                       h lell)
               (inc n))))))

(def src-0 [#{[:h :h] [:l :h]}
            #{[:h :g]}
            #{[:l :g]}
            #{}])
(def src-1 [#{[:t :g] [:t :h] [:l :g] [:s :g]}
            #{[:l :h] [:s :h]}
            #{[:p :g] [:p :h] [:r :g] [:r :h]}
            #{}])
(def src-2 [#{[:t :g] [:t :h] [:l :g] [:s :g] [:e :g] [:e :h] [:d :g] [:d :h]}
            #{[:l :h] [:s :h]}
            #{[:p :g] [:p :h] [:r :g] [:r :h]}
            #{}])

(find-min src-1) ;part 1
(find-min src-2) ;part 2
