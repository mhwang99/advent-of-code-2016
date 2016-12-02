(defn gethead
  [current direction]
  (case current
    :n (case direction \L :w :e)
    :e (case direction \L :n :s)
    :s (case direction \L :e :w)
    :w (case direction \L :s :n)))

(defn getlen
  [[x y]]
  (let [x (if (< x 0) (- x) x)
        y (if (< y 0) (- y) y)]
    (+ x y)))

(defn part1 [lst]
  (getlen
    (loop [head :n
           x 0 y 0
           entity (first lst)
           lst (rest lst)]
      (if (nil? entity) [x y]
        (let [direction (first entity)
              n (Integer. (apply str (rest entity)))
              head (gethead head direction)]
          (case head
            :n (recur :n x (+ y n) (first lst) (rest lst))
            :s (recur :s x (- y n) (first lst) (rest lst))
            :e (recur :e (+ x n) y (first lst) (rest lst))
            :w (recur :w (- x n) y (first lst) (rest lst))))))))

(part1

["L3" "R2" "L5" "R1" "L1" "L2" "L2" "R1" "R5" "R1" "L1" "L2" "R2" "R4" "L4" "L3" "L3" "R5" "L1" "R3" "L5" "L2" "R4" "L5" "R4" "R2" "L2" "L1" "R1" "L3" "L3" "R2" "R1" "L4" "L1" "L1" "R4" "R5" "R1" "L2" "L1" "R188" "R4" "L3" "R54" "L4" "R4" "R74" "R2" "L4" "R185" "R1" "R3" "R5" "L2" "L3" "R1" "L1" "L3" "R3" "R2" "L3" "L4" "R1" "L3" "L5" "L2" "R2" "L1" "R2" "R1" "L4" "R5" "R4" "L5" "L5" "L4" "R5" "R4" "L5" "L3" "R4" "R1" "L5" "L4" "L3" "R5" "L5" "L2" "L4" "R4" "R4" "R2" "L1" "L3" "L2" "R5" "R4" "L5" "R1" "R2" "R5" "L2" "R4" "R5" "L2" "L3" "R3" "L4" "R3" "L2" "R1" "R4" "L5" "R1" "L5" "L3" "R4" "L2" "L2" "L5" "L5" "R5" "R2" "L5" "R1" "L3" "L2" "L2" "R3" "L3" "L4" "R2" "R3" "L1" "R2" "L5" "L3" "R4" "L4" "R4" "R3" "L3" "R1" "L3" "R5" "L5" "R1" "R5" "R3" "L1"])

(defn genpath
  [[x y] head n]
  (let [i (range 1 (inc n))]
    (case head
      :n (map #(vector x (+ y %)) i)
      :s (map #(vector x (- y %)) i)
      :e (map #(vector (+ x %) y) i)
      :w (map #(vector (- x %) y) i))))

(defn markpath
  [head n history]
  (let [path (genpath (last history) head n)]
    (loop [point (first path)
           path (rest path)
           history history]
      (if (nil? point) [history nil]
        (if (some #{point} history) [nil point]
          (recur (first path) (rest path) (conj history point)))))))

(defn part2 [lst]
  (getlen
    (loop [head :n
           entity (first lst)
           lst (rest lst)
           history [[0 0]]]
      (if (nil? entity) [0 0]
        (let [direction (first entity)
              n (Integer. (apply str (rest entity)))
              head (gethead head direction)
              [new-history cross] (markpath head n history)]
          (if (nil? new-history) cross
            (recur head (first lst) (rest lst) new-history)))))))


(part2

["L3" "R2" "L5" "R1" "L1" "L2" "L2" "R1" "R5" "R1" "L1" "L2" "R2" "R4" "L4" "L3" "L3" "R5" "L1" "R3" "L5" "L2" "R4" "L5" "R4" "R2" "L2" "L1" "R1" "L3" "L3" "R2" "R1" "L4" "L1" "L1" "R4" "R5" "R1" "L2" "L1" "R188" "R4" "L3" "R54" "L4" "R4" "R74" "R2" "L4" "R185" "R1" "R3" "R5" "L2" "L3" "R1" "L1" "L3" "R3" "R2" "L3" "L4" "R1" "L3" "L5" "L2" "R2" "L1" "R2" "R1" "L4" "R5" "R4" "L5" "L5" "L4" "R5" "R4" "L5" "L3" "R4" "R1" "L5" "L4" "L3" "R5" "L5" "L2" "L4" "R4" "R4" "R2" "L1" "L3" "L2" "R5" "R4" "L5" "R1" "R2" "R5" "L2" "R4" "R5" "L2" "L3" "R3" "L4" "R3" "L2" "R1" "R4" "L5" "R1" "L5" "L3" "R4" "L2" "L2" "L5" "L5" "R5" "R2" "L5" "R1" "L3" "L2" "L2" "R3" "L3" "L4" "R2" "R3" "L1" "R2" "L5" "L3" "R4" "L4" "R4" "R3" "L3" "R1" "L3" "R5" "L5" "R1" "R5" "R3" "L1"])

