

(import 'java.security.MessageDigest
        'java.math.BigInteger)

(defn md5 [s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        size (* 2 (.getDigestLength algorithm))
        raw (.digest algorithm (.getBytes s))
        sig (.toString (BigInteger. 1 raw) 16)
        padding (apply str (repeat (- size (count sig)) "0"))]
    (str padding sig)))

(defn mdm5
  [s n mdm part]
  (let [s (str s n)]
    (if-let [r (get mdm n)]
      [mdm r]
      (let [r (if (= part 1) (md5 s)
                (reduce #(if (= %2 2016) (reduced (md5 %1)) (md5 %1))
                      s (range)))
            mdm (assoc mdm n r)]
        [mdm r]))))


(defn fnd3
  [s n mdm part]
  (let [[mdm hs] (mdm5 s n mdm part)
        [p c] (reduce (fn [[p i] c]
                        (if (= p c)
                          (if (= i 2) (reduced [p 3])
                            [p (inc i)])
                          [c 1])) [nil 0] hs)]
    (if (= c 3) [mdm p]
      [mdm nil])))

(defn fnd5
  [s n g mdm part]
  (let [[mdm hs] (mdm5 s n mdm part)
        c (reduce (fn [i c]
                        (if (= g c)
                          (if (= i 4) (reduced 5)
                            (inc i))
                          0)) 0 hs)]
    (if (= c 5) [mdm true]
      [mdm false])))

(defn proc
  [s gg part]
  (loop [n 2 fth 0 mdm {}]
    (let [[mdm c] (fnd3 s n mdm part)]
      (if (nil? c) (recur (inc n) fth mdm)
        (let [[mdm b] (reduce (fn [[mdm b] i]
                               (let [[mdm f5] (fnd5 s (+ n 1 i) c mdm part)]
                                 (if f5 (reduced [mdm true])
                                   [mdm false])))
                              [mdm false] (range 1000))]
          (if b
            (if (= fth (dec gg)) n
              (recur (inc n) (inc fth) mdm))
            (recur (inc n) fth mdm)))))))

;; part1
(proc "qzyelonm" 64 1)

;; part1
(proc "qzyelonm" 64 2)


