(ns net.wmorgan.coll
  (:use [clojure.test]
        [clojure.contrib.seq :only [indexed]]))

(with-test
  (defn outer-product
    [coll-a coll-b]
    (for [a coll-a b coll-b]
      [a b]))

  (is (= [[1 3] [1 4] [2 3] [2 4]] (outer-product [1 2] [3 4])))
  (is (empty? (outer-product [1 2 3] []))))

(with-test
  (defn palindromic?
    [n]
    (= (seq (str n)) (reverse (str n))))

  (is (palindromic? 202))
  (is (palindromic? 64322346))
  (is (not (palindromic? 832))))

(with-test
  (defn unfrequencies
    [freqs]
    (for [[element qty] freqs
          el (repeat qty element)]
      el))

  (is (= [4 4 4] (unfrequencies {4 3})))
  (is (= 12 (count (unfrequencies {1 6 4 6}))))
  (is (= [] (unfrequencies {}))))

(with-test
  (defn disjoint?
    [s t]
    (not-any? #(contains? s %) t))

  (is (disjoint? #{1 2} #{3 4}))
  (is (not (disjoint? #{1 2 3} #{3 4 5})))
  (is (disjoint? #{} #{}))
  (is (disjoint? #{} #{1 2 3})))

(with-test
  (defn xor
    [& bools]
    (->> bools
         (map #(if % 1 0))
         (reduce +)
         odd?))

  (is (xor true false))
  (is (not (xor true true)))
  (is (not (xor false false)))
  (is (xor nil 7))
  (is (xor true false true false true)))

(with-test
  (defn map-fn
    [f coll]
    (->> (map #(vector % (f %)) coll)
         (into {})))

  (is (= {1 2 2 3 3 4 4 5} (map-fn inc [1 2 3 4]))))

(with-test
  (defn nontrivial-map-fixed-points
    [m]
    (for [[k v] m :when (not= k v)
                  :when (= (m v) k)]
      k))

  (is (= [] (nontrivial-map-fixed-points {:a :a :b :c :d :e})))
  (is (= #{:a :b} (set (nontrivial-map-fixed-points {:a :b :b :a :c :d}))))
  (is (= [] (nontrivial-map-fixed-points {:a :b :b :c :c :d :d :a}))))

(with-test
  (defn repeat-length
    [coll]
    (loop [[[idx el] & rest] (indexed coll)
           seen {}]
      (if-let [prev-idx (seen el)]
        (- idx prev-idx)
        (if rest
          (recur rest (assoc seen el idx))))))

  (is (= 1 (repeat-length [1 1 0 1 2 3])))
  (is (= 3 (repeat-length (cycle [1 2 3]))))
  (is (nil? (repeat-length [])))
  (is (nil? (repeat-length [1 2 3 4]))))

(with-test
  (defn max-index
    [coll]
    (->> (indexed coll)
         (apply max-key second)
         first))

  (is (= 0 (max-index [5 4 3 2 1])))
  (is (= 2 (max-index [1 2 3 2 1]))))
