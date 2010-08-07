(ns euler.problems
  (:require [clojure.contrib.duck-streams :as ds]
            [clojure.contrib.string :as string])
  (:use [euler.english]
        [net.wmorgan num coll string plane]
        [euler.util :only [defproblem]]
        [clojure.contrib.seq :only [indexed find-first]]))

(defproblem 1
  (->> (range 1000)
       (filter #(or (divides? 3 %) (divides? 5 %)))
       (reduce +)))

(defproblem 2
  (->> fibs
       (take-while #(<= % 2000000))
       (filter even?)
       (reduce +)))

(defproblem 3
  (->> 600851475143 prime-factors last))

(defproblem 4
  (->> (outer-product (range 100 1000) (range 100 1000))
       (map #(apply * %))
       (filter palindromic?)
       (reduce max)))

(defproblem 5
  (->> (range 1 21)
       (map prime-factors)
       (map frequencies)
       (apply merge-with max)
       unfrequencies
       (reduce *)))

(defproblem 6
  (- (square (reduce + (range 1 101)))
     (reduce + (map square (range 1 101)))))

(defproblem 7
  (nth primes 10000))

(defproblem 8
  (->> (ds/read-lines "rsrc/problem8.txt")
       (apply concat)
       (map char->num)
       (partition 5 1)
       (map #(reduce * %))
       (reduce max)))

(defproblem 9
  (->> pythagorean-triples
       (map #(reduce + %))
       (find-first #(= 1000 %))
       (reduce *)))

(defproblem 10
  (->> primes
       (take-while #(< % 2000000))
       (reduce +)))

(defproblem 11
  (->> (ds/read-lines "rsrc/problem11.txt")
       parse-numbers-2d
       repeat
       (mapcat colls-rotation [0 90 45 -45])
       (mapcat #(partition 4 1 %))
       (map #(reduce * %))
       (reduce max)))

(defproblem 12
  (->> triangle-numbers
       (map fast-divisor-count)
       (take-while #(< % 500))
       count
       (nth triangle-numbers)))

(defproblem 13
  (->> (ds/read-lines "rsrc/problem13.txt")
       (map bigint)
       (reduce +)
       str
       (string/take 10)))

(defproblem 14
  (->> (range 1000000)
       (map collatz-length)
       indexed
       (apply max-key second)
       first))

(defproblem 15
  (choose 40 20)) 

(defproblem 16
  (->> (nth powers-of-two 1000)
       str
       (map char->num)
       (reduce +)))

(defproblem 17
  (->> (range 1 1001)
       (mapcat num->english)
       (remove #{\- \space})
       count))

(defproblem 18
  (->> (ds/read-lines "rsrc/problem18.txt")
       parse-numbers-2d
       triangle-routes
       (map #(reduce + %))
       (reduce max)))

