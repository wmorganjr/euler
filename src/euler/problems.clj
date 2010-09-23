(ns euler.problems
  (:import java.util.Calendar)
  (:require [clojure.contrib.duck-streams :as ds]
            [clojure.contrib.string :as string]
            [clojure.set :as set])
  (:use [euler.english]
        [net.wmorgan num coll string plane]
        [euler.util :only [defproblem]]
        [clojure.contrib.combinatorics :only [selections lex-permutations]]
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
  (->> (range 100000)
       (map collatz-length)
       max-index))

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

(defn day-inc
  [^Calendar cal]
  (doto (Calendar/getInstance)
    (.set (.get cal Calendar/YEAR)
          (.get cal Calendar/MONTH)
          (.get cal Calendar/DAY_OF_MONTH))
    (.add Calendar/DAY_OF_MONTH 1)))

(defproblem 19
  (->> (doto (Calendar/getInstance)
         (.set 1901 0 1))
       (iterate day-inc)
       (take-while #(< (.get % Calendar/YEAR) 2001))
       (filter #(= 1 (.get % Calendar/DAY_OF_MONTH)))
       (filter #(= Calendar/SUNDAY (.get % Calendar/DAY_OF_WEEK)))
       count))

(defproblem 20
  (->> (factorial 100)
       str
       (map char->num)
       (reduce +)))

(defproblem 21
  (->> (range 1 10000)
       (map-fn sum-of-divisors)
       nontrivial-map-fixed-points
       (reduce +)))

(defn alphabetic-value
  [string]
  (->> (map #(inc (- (int %) (int \A))) string)
       (reduce +)))

(defn parse-names
  [line]
  (->> (.split line ",")
       (map #(re-matches #"\"(.*)\"" %))
       (map second)))

(defproblem 22
  (->> (slurp "rsrc/problem22.txt")
       parse-names
       sort
       (map alphabetic-value)
       (map * (iterate inc 1))
       (reduce +)))

(defn pairs-under-limit
  [n coll]
  (for [[i a] (indexed coll) :while (< a n)
        b (drop i coll) :while (< (+ a b) n)]
    [a b]))

(defproblem 23
  (->> abundant-numbers
       (pairs-under-limit 28124)
       (map #(reduce + %))
       set
       (set/difference (set (range 1 28124)))
       (reduce +)))

(defproblem 24
  (->> (range 10)
       lex-permutations
       (take 1000000)
       last))

(defproblem 25
  (->> fibs0
       (take-while #(< (count (str %)) 1000))
       count
       inc))

(defproblem 26
  (->> (range 1 1001)
       (map inverse-division)
       (map repeat-length)
       max-index
       inc))

(def candidate-quadratic-pairs
  (for [a (range -999 1000) :when (odd? a)
        b primes :while (< b 1000)]
    [a b]))

(defproblem 27
  (->> candidate-quadratic-pairs
       (map #(apply quadratic-fn %))
       (map #(map % (range)))
       (map #(take-while prime? %))
       (map count)
       max-index
       (nth candidate-quadratic-pairs)
       (apply *)))

(defproblem 28
  (->> (iterate #(+ 2 %) 2)
       (map #(repeat 4 %))
       (take 500)
       (apply concat)
       (reductions + 1)
       (reduce +)))

