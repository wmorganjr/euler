(ns net.wmorgan.plane
  (:use clojure.test
        net.wmorgan.string
        [clojure.contrib.combinatorics :only [selections]]))

(with-test
  (defn parse-numbers-2d
    [lines]
    (->> (map words lines)
         (map #(map chars->num %))))

  (is (= [[1 2] [3 4]] (parse-numbers-2d ["01 02" "03 04"]))))

(with-test
  (defn triangle-routes
    [colls]
    (->> (dec (count colls))
         (selections [0 1])
         (map #(reductions + 0 %))
         (map #(map nth colls %)))))

(defmulti colls-rotation (fn [degrees colls] degrees))

(defmethod colls-rotation 0
  [_ colls]
  colls)

(defmethod colls-rotation 90
  [_ colls]
  (map (fn [n _]
         (map nth colls (repeat n)))
       (iterate inc 0)
       (first colls)))

(defmethod colls-rotation -90
  [_ colls]
  (->> colls (map reverse) (colls-rotation 90)))

(defmethod colls-rotation 45
  [_ colls]
  (map (fn [n _]
         (->> (range n -1 -1)
              (map #(nth %1 %2 ::not-found) colls)
              (remove #{::not-found})))
       (iterate inc 0)
       (rest (concat colls colls))))

(defmethod colls-rotation -45
  [_ colls]
  (->> colls (map reverse) (colls-rotation 45)))

(defmethod colls-rotation 180
  [_ colls]
  (reverse (map reverse colls)))

(testing "Two-dimensional rotating"
  (let [sq [[1 2] [3 4]]]
    (is (= [[1 2] [3 4]] (colls-rotation 0 sq)))
    (is (= [[1 3] [2 4]] (colls-rotation 90 sq)))
    (is (= [[2 4] [1 3]] (colls-rotation -90 sq)))
    (is (= [[4 3] [2 1]] (colls-rotation 180 sq)))
    (is (= [[1] [2 3] [4]] (colls-rotation 45 sq)))
    (is (= [[2] [1 4] [3]] (colls-rotation -45 sq)))))

