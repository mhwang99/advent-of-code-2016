
(def src (->> (str/split (slurp "day22.txt") #"\n")
              (mapv
                (fn[s]
                  (map #(Integer. %) (drop-last 1 (drop 3 (re-seq #"\d+" s))))))))

(defn viable
  [l [a b]]
  (let [a (nth l a)
        b (nth l b)]
    (if (or (= 0 (nth a 0))
            (> (nth a 0) (nth b 1))) false
      true)))

(defn redraw
  [l ylen mx]
  (let [[a b] (get l (- mx ylen))
        v (+ a b)]
    (-> (mapv (fn [[a b]]
            (cond
              (= 0 a) \z
              (> (+ a b) (* v 2)) \#
              :else \.)) l)
        (assoc (- mx ylen) \g)
        )))

(def re-src (redraw src 25 (count src)))

(defn get-nxtpath
  [p l ylen mx his]
  (let [bs (cond-> []
             (>= (- p ylen) 0) (conj (- p ylen))
             (< (+ p ylen) mx) (conj (+ p ylen))
             (> (mod p ylen) 0) (conj (dec p))
             (> (mod (inc p) ylen) 0) (conj (inc p)))]
    (->> bs
        (filter (complement his))
        (filter #(not (#{\g \#} (get l %)))))))

(defn get-path
  [l ylen]
  (let [mx (count l)
        p (.indexOf l \g)]
    (loop [his #{p} paths [[p]]]
      (let [[his paths] (reduce (fn [[his paths] path]
                                  (let [ps (get-nxtpath (last path) l ylen mx his)
                                        his (into his ps)]
                                    [his (into paths (mapv (partial conj path) ps))]))
                                [his []] paths)
            goals (reduce #(case (last %2) 0 (conj %1 %2) %1) [] paths)]
        (if (empty? goals) (recur his paths)
          goals)))))

(defn get-minmove
  [l ylen mx init target]
  (loop [his #{init} ps [init] n 0]
    (let [[his ps] (reduce (fn [[his ps] p]
                             (let [cps (get-nxtpath p l ylen mx his)
                                   ps (into ps cps)
                                   his (into his ps)]
                               [his ps]))
                           [his []] ps)]
      (cond
        (empty? ps) -1
        (some #{target} ps) (inc n)
        :else (recur his ps (inc n))))))

(defn proc-path
  [l ylen mx path]
  (loop [l l path path cnt 0]
    (if (empty? path) cnt
      (let [g (.indexOf l \g)
            init (.indexOf l \z)
            target (first path)
            n (get-minmove l ylen mx init target)]
        (recur (-> l
                   (assoc init \.)
                   (assoc target \g)
                   (assoc g \z))
               (rest path)
               (+ cnt n 1))))))

(defn proc-p1
  [l]
  (->> (for [a (range (count l))
             b (range (count l))
             :when (not= a b)]
         [a b])
       (filter #(viable l %))
       count))

(defn proc-p2
  [l ylen]
  (let [mx (count l)
        paths (get-path l ylen)]
    (apply min (mapv #(proc-path l ylen mx (rest %)) paths))))


;part1
(proc-p1 src)

;part2
(proc-p2 re-src 25)

