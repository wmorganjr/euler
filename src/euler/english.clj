(ns euler.english
  (:use clojure.contrib.def
        clojure.test))

(defvar- number-words
  {1 "one"
   2 "two"
   3 "three"
   4 "four"
   5 "five"
   6 "six"
   7 "seven"
   8 "eight"
   9 "nine"
   10 "ten"
   11 "eleven"
   12 "twelve"
   13 "thirteen"
   14 "fourteen"
   15 "fifteen"
   16 "sixteen"
   17 "seventeen"
   18 "eighteen"
   19 "nineteen"
   20 "twenty"
   30 "thirty"
   40 "forty"
   50 "fifty"
   60 "sixty"
   70 "seventy"
   80 "eighty"
   90 "ninety"
   1000 "one thousand"})

(defn- number-sorter
  [n]
  (cond
    (contains? number-words n) :atom
    (< n 100) :tens
    (zero? (mod n 100)) :even-hundred
    :else :hundreds))

(defmulti num->english number-sorter)

(defmethod num->english :atom
  [n]
  (number-words n))

(defmethod num->english :tens
  [n]
  (let [ones (mod n 10)
        tens (- n ones)]
    (str (num->english tens) "-" (num->english ones))))

(defmethod num->english :even-hundred
  [n]
  (str (num->english (/ n 100)) " hundred"))

(defmethod num->english :hundreds
  [n]
  (let [tens (mod n 100)
        hundreds (- n tens)]
    (str (num->english hundreds) " and " (num->english tens))))

(testing "Number to English converstion"
  (is (= (num->english 7) "seven"))
  (is (= (num->english 21) "twenty-one"))
  (is (= (num->english 451) "four hundred and fifty-one"))
  (is (= (num->english 700) "seven hundred"))
  (is (= (num->english 1000) "one thousand")))

