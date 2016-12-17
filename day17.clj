
(defn get-next
  [x y c s d]
  (cond
    (or (< x 0) (< y 0)
        (> x 3) (> y 3)) nil
    (#{\b \c \d \e \f} c) [[x y] (str s d)]
    :else nil))

(defn get-nexts
  [[x y] s]
  (let [h (take 4 (md5 s))]
    (filter identity
            (map (fn [c [ix iy] d]
                   (get-next (+ ix x) (+ iy y) c s d))
                 h
                 [[0 -1] [0 1] [-1 0] [1 0]]
                 "UDLR"))))

(defn some-ps
  [l g]
  (reduce (fn [r [p s]]
            (if (= p g) (reduced s)
              r)) nil l))

(defn proc
  [s part]
  (loop [l [[[0 0] s]]]
    (let [nl (reduce (fn [nl [p s]]
                       (concat nl (get-nexts p s)))
                     [] l)]
      (if (empty? nl) "NOT FOUND"
        (if-let [s (some-ps nl [3 3])]
          (let [path (re-find #"[A-Z]+" s)]
            (if (= part 1) path
              (do (println "FOUND" (count path))
                  (recur (filter #(not= (first %) [3 3]) nl)))))
          (recur nl))))))

;part 1
(proc "ihgpwlah" 1)

;part 2
(proc "ihgpwlah" 2)
