(ns euler.util
  (:import java.util.concurrent.TimeUnit))

(defmacro with-timeout
  [seconds & body]
  `(let [f# (future ~@body)]
     (.get f# ~seconds TimeUnit/SECONDS)))

(defmacro defproblem
  [n & body]
  `(defn ~(symbol (str "problem-" n))
     [& [timeout#]]
     (time (with-timeout (or timeout# 10)
                         ~@body))))

