
(defn swp-pos
  [s & l]
  (let [[a b] (sort l)]
    (concat (take a s)
            [(nth s b)]
            (take (- b a 1) (drop (inc a) s))
            [(nth s a)]
            (drop (inc b) s))))

(defn swp-chr
  [s a b]
  (let [a (.indexOf s (first a))
        b (.indexOf s (first b))]
    (swp-pos s a b)))

(defn rot-r
  [s a]
  (let [a (mod a (count s))]
    (concat (take-last a s)
            (drop-last a s))))

(defn rot-l
  [s a]
  (let [a (mod a (count s))]
    (concat (drop a s)
            (take a s))))

(defn rot-b
  [s a]
  (let [a (.indexOf s (first a))
        a (+ a 1 (if (> a 3) 1 0))]
    (rot-r s a)))

(defn rot-rb
  [s a]
  (let [a (.indexOf s (first a))
        a ({0 9, 1 1,
            2 6, 3 2,
            4 7, 5 3,
            6 8, 7 4} a)]
    (rot-l s a)))

(defn rev-pos
  [s & l]
  (let [[a b] (sort l)]
    (concat (take a s)
            (reverse (drop a (take (inc b) s)))
            (drop (inc b) s))))

(defn mov-pos
  [s a b]
  (let [c (nth s a)
        s (concat (take a s) (drop (inc a) s))]
    (concat (take b s)
            [c]
            (drop b s))))

(defn nthi
  [l i]
  (Integer. (nth l i)))

(defn proc
  [l s]
  (let [s (seq s)]
    (apply str
           (reduce (fn [s cmd]
                     (case [(first cmd) (second cmd)]
                       ["swap" "position"] (swp-pos s (nthi cmd 2) (nthi cmd 5))
                       ["swap" "letter"] (swp-chr s (nth cmd 2) (nth cmd 5))
                       ["rotate" "left"] (rot-l s (nthi cmd 2))
                       ["rotate" "right"] (rot-r s (nthi cmd 2))
                       ["rotate" "based"] (rot-b s (nth cmd 6))
                       ["reverse" "positions"] (rev-pos s (nthi cmd 2) (nthi cmd 4))
                       ["move" "position"] (mov-pos s (nthi cmd 2) (nthi cmd 5))
                       s))
                   s l))))

(defn rev-proc
  [l s]
  (let [s (seq s)
        l (reverse l)]
    (apply str
           (reduce (fn [s cmd]
                     (case [(first cmd) (second cmd)]
                       ["swap" "position"] (swp-pos s (nthi cmd 2) (nthi cmd 5))
                       ["swap" "letter"] (swp-chr s (nth cmd 2) (nth cmd 5))
                       ["rotate" "left"] (rot-r s (nthi cmd 2))
                       ["rotate" "right"] (rot-l s (nthi cmd 2))
                       ["rotate" "based"] (rot-rb s (nth cmd 6))
                       ["reverse" "positions"] (rev-pos s (nthi cmd 2) (nthi cmd 4))
                       ["move" "position"] (mov-pos s (nthi cmd 5) (nthi cmd 2))
                       s))
                   s l))))

(def cmds (mapv #(str/split % #"\s+") (str/split (slurp "day21.txt") #"\n")))

;part1
(proc cmds "abcdefh")
;part2
(rev-proc cmds "fbgdceah")
