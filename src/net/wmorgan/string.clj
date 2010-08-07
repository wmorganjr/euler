(ns net.wmorgan.string
  (:use clojure.test))

(with-test
  (defn char->num
    [c]
    (Integer/parseInt (str c)))

  (is (= (char->num \0) 0))
  (is (= (char->num \7) 7))
  (is (thrown? NumberFormatException (char->num \t))))

(with-test
  (defn chars->num
    [cs]
    (->> (apply str cs)
         Integer/parseInt))

  (is (= (chars->num "54") 54))
  (is (= (chars->num [5 4]) 54))
  (is (= (chars->num [\5 \4]) 54))
  (is (thrown? NumberFormatException (chars->num "4a"))))
      
(with-test
  (defn words
    [s]
    (-> (.trim s)
        (.split "\\s+")
        seq))

  (is (= (words "foo") ["foo"]))
  (is (= (words "foo bar") ["foo" "bar"]))
  (is (= (words "  foo bar") ["foo" "bar"]))
  (is (= (words "foo bar   ") ["foo" "bar"]))
  (is (= (words "foo\t bar   ") ["foo" "bar"])))

