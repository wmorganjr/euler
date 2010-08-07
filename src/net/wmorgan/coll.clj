(ns net.wmorgan.coll
  (:use clojure.test))

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

