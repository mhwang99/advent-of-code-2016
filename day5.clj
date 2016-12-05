(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn getc
  [s n]
  (loop [n n]
    (let [h (md5 (str s n))]
      (if (.startsWith h "00000")
        (let [h (drop 5 h)]
          (vector n (first h) (first (rest h))))
        (recur (inc n))))))

(defn part1
  [s]
  (loop [n 0 r []]
    (let [[n c _]  (getc s n)]
      (println n c)
      (if (= (count r) 7) (conj r c)
        (recur (inc n) (conj r c))))))

(defn setr
  [r i c]
  (let [i (try (Integer. (str i)) (catch Throwable e 10))]
    (cond
      (> i 7) r
      (get r i) r
      :else (assoc r i c))))

(defn part2
  [s]
  (loop [n 0 r {}]
    (let [[n i c] (getc s n)]
      (println n i c)
      (let [r (setc r i c)]
        (if (= (count r) 8) (map second (sort #(compare (first %1)(first %2)) r))
          (recur (inc n) r))))))

(part1 "uqwqemis")
(part2 "uqwqemis")

