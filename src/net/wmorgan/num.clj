(ns net.wmorgan.num
  (:use [net.wmorgan coll plane]
        clojure.test
        [clojure.contrib.seq-utils :only [find-first]]))

(with-test
  (defn divides?
    [d n]
    (zero? (mod n d)))
  
  (is (divides? 5 15))
  (is (not (divides? 2 7)))
  (is (thrown? ArithmeticException (divides? 0 10))))

(with-test
  (def fibs (lazy-cat [1 2] (map + fibs (rest fibs))))

  (is (= [1 2 3 5 8] (take 5 fibs))))

(defn square [n] (* n n))

(with-test
  (declare primes)

  (is (= [2 3 5 7 11] (take 5 primes))))

(with-test
  (defn least-nontrivial-divisor
    [n]
    (->> primes
         (take-while #(<= (square %) n))
         (find-first #(divides? % n))))

  (is (= 2 (least-nontrivial-divisor 4)))
  (is (= 5 (least-nontrivial-divisor 55)))
  (is (nil? (least-nontrivial-divisor 13)))
  (is (nil? (least-nontrivial-divisor 1))))
  
(with-test
  (defn prime?
    [n]
    (->> (least-nontrivial-divisor n)
         nil?
         (and (> n 1))))

  (is (prime? 17))
  (is (not (prime? 21)))
  (is (not (prime? 1)))
  (is (not (prime? 0))))

(def primes (lazy-cat [2] (filter prime? (iterate inc 3))))

(with-test
  (defn divisions
    [n d]
    (->> (iterate #(/ % d) n)
         (take-while integer?)
         rest))

  (is (= [16 8 4 2 1] (divisions 32 2)))
  (is (= [100 20 4] (divisions 500 5)))
  (is (empty? (divisions 7 2))))

(with-test
  (defn prime-factors
    [n]
    (when (> n 1)
      (if-let [p (least-nontrivial-divisor n)]
        (let [divs (divisions n p)]
          (lazy-cat (repeat (count divs) p)
                    (prime-factors (last divs))))
        [n])))

  (is (= [2 2 2 3] (prime-factors 24)))
  (is (= [19] (prime-factors 19)))
  (is (empty? (prime-factors 1))))

(with-test
  (defn coprime?
    [m n]
    (disjoint? (set (prime-factors m))
               (set (prime-factors n))))

  (is (coprime? 7 2))
  (is (coprime? 1 32))
  (is (coprime? 32 81))
  (is (not (coprime? 32 192)))
  (is (not (coprime? 7 49))))

(with-test
  (def primitive-pythagorean-triples
    (for [m (iterate inc 1)
          n (range 1 m) :when (coprime? m n)
                        :when (xor (even? m) (even? n))]
      [(- (square m) (square n)) (* 2 m n) (+ (square m) (square n))]))

  (is (= [[3 4 5] [5 12 13] [15 8 17]]
         (take 3 primitive-pythagorean-triples))))

(with-test
  (defn pythagorean-multiples
    [triple]
    (for [multiple (iterate inc 1)]
      (map #(* multiple %) triple)))

  (is (= [[3 4 5] [6 8 10] [9 12 15]]
         (take 3 (pythagorean-multiples [3 4 5])))))

(with-test
  (def pythagorean-triples
    (->> primitive-pythagorean-triples
         (map pythagorean-multiples)
         (colls-rotation 45)
         (apply concat)))

  (is (= [[3 4 5] [6 8 10] [5 12 13]]
         (take 3 pythagorean-triples))))

(with-test
  (defn divisors
    [n]
    (->> (range 1 (inc n))
         (filter #(divides? % n))))

  (is (= [1 2 4 7 14 28] (divisors 28)))
  (is (= [1 2 3 4 6 12] (divisors 12)))
  (is (= [1] (divisors 1)))
  (is (= [1 19] (divisors 19))))

(with-test
  (defn fast-divisor-count
    [n]
    (->> (prime-factors n)
         frequencies
         vals
         (map inc)
         (reduce *)))

  (is (= 1 (fast-divisor-count 1)))
  (is (= 6 (fast-divisor-count 12)))
  (is (= 24 (fast-divisor-count 360))))

(with-test
  (def triangle-numbers (reductions + 1 (iterate inc 2)))

  (is (= [1 3 6 10 15] (take 5 triangle-numbers))))

(with-test
  (defn factorial
    [n]
    (reduce * (range 1 (inc n))))

  (is (= 120 (factorial 5)))
  (is (= 1 (factorial 1)))
  (is (= 1 (factorial 0))))

(with-test
  (defn falling-factorial
    [n k]
    (reduce * (range n (- n k) -1)))

  (is (= 20 (falling-factorial 5 2)))
  (is (= 6 (falling-factorial 3 3)))
  (is (= 17 (falling-factorial 17 1)))
  (is (= 1 (falling-factorial 3 0)))
  (is (= 1 (falling-factorial 0 0))))

(with-test

  (defn choose
    [n k]
    (/ (falling-factorial n k)
       (factorial k)))

  (is (= 10 (choose 5 3)))
  (is (= 10 (choose 5 2)))
  (is (= 1 (choose 5 5)))
  (is (= 5 (choose 5 1))))

(with-test
  (def powers-of-two (iterate #(* 2 %) 1))

  (is (= [1 2 4 8 16] (take 5 powers-of-two))))

(with-test
  (defn collatz
    [n]
    (if (even? n)
      (/ n 2)
      (inc (* 3 n))))

  (is (= 8 (collatz 16)))
  (is (= 22 (collatz 7))))

(with-test
  (defn collatz-length
    [n]
    (->> n (iterate collatz) (take-while #(> % 1)) count inc))

  (is (= 10 (collatz-length 13)))
  (is (= 1 (collatz-length 1))))

