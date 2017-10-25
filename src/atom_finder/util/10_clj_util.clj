(in-ns 'atom-finder.util)

(defmacro %w [& words]
    `(list ~@(map str (vec words))))

(defn tap [f x] (f x) x)
(defn pap
  "print the value of the argument and return it; optionally modified by a function"
  ([x] (tap prn x))
  ([f x] (tap (comp prn f) x)))

(defn =by
  "Test if arguments are equal after applying f to all of them"
  [f & args]
  (apply = (map f args)))

;; https://github.com/clojure/clojure/blob/clojure-1.9.0-alpha14/src/clj/clojure/core.clj#L1683
(defmacro juxt->>
  "Applies the first argument as the last argument to each form in parrallel
   returning the results in a vector"
  [x & forms]
  (cons vector
        (for [form forms]
          (if (seq? form)
            (with-meta `(~(first form) ~@(next form) ~x) (meta form))
            (list form x)))))

; print the name and value of an expression
(defmacro pprn [x]
  `(let [y# ~x]
    (do
      (print (str ~(str x ": ") (prn-str y#)))
      y#)))

(defn trunc [s n]
    (subs s 0 (min (count s) n)))

(def range-from (partial iterate inc))

(defn count-lines [str]
    (count (filter #{\newline} str)))

(defn line-range [min max s]
  "return the line of a string between [min max)"
  (->> s str/split-lines
       (drop (dec min))
       (take (- max min))))

(defn close?
  "Are two numbers approximately equal"
  [tolerance x y]
     (< (Math/abs (- x y)) tolerance))

; https://rosettacode.org/wiki/Detect_division_by_zero#Clojure
(defn safe-div [x y]
  (if (and x y)
    (try (/ x y)
         (catch ArithmeticException _
                                        ;(println "Division by zero caught!")
           (cond (> x 0)   Double/POSITIVE_INFINITY
                 (zero? x) Double/NaN
                 :else     Double/NEGATIVE_INFINITY)))
    Double/NaN))

(defn arg-count [f]
  (let [m (first (.getDeclaredMethods (class f)))
        p (.getParameterTypes m)]
    (alength p)))

;; http://stackoverflow.com/questions/23178750/iteratively-apply-function-to-its-result-without-generating-a-seq
(defn fn-pow
  [f x n]
    (nth (iterate f x) n))

;; https://stackoverflow.com/questions/44166399/clojure-equivalent-of-scalas-andthen
(defn and-then [& fns] (apply comp (reverse fns)))

; https://gist.github.com/micmarsh/bcbe19c9de8bb7a471bf
(defn flip [function]
  (fn
    ([] (function))
    ([x] (function x))
    ([x y] (function y x))
    ([x y z] (function z y x))
    ([a b c d] (function d c b a))
    ([a b c d & rest]
     (->> rest
          (concat [a b c d])
          reverse
          (apply function)))))

(defn errln [& s]
  "println to stderr"
  (binding [*out* *err*]
      (println (apply str s))))

(defn join-keywords
  ([sep kws] (->> kws (map name) (clojure.string/join sep) keyword))
  ([kws] (join-keywords "" kws)))

; https://stackoverflow.com/questions/11676120/why-dont-when-let-and-if-let-support-multiple-bindings-by-default/36160972#36160972
(defmacro if-let*
  ([bindings then]
   `(if-let* ~bindings ~then nil))
  ([bindings then else]
   (if (seq bindings)
     `(if-let [~(first bindings) ~(second bindings)]
        (if-let* ~(drop 2 bindings) ~then ~else)
        ~else)
          then)))

(defmacro when-let*
  [bindings & then]
  (if (seq bindings)
    `(when-let [~(first bindings) ~(second bindings)]
      (when-let* ~(drop 2 bindings) ~@then))
     `(do ~@then)))

(defn bin "Convert a value to 1 or 0 based on its truthiness"
  [bool] (if bool 1 0))


(defn flatcat
  "Take two lists/items and merge them"
  [a b]
  (cond
    (and (sequential? a) (sequential? b)) (concat a b)
    (sequential? a) (conj a b)
    (sequential? b) (conj b a)
    :default [a b]))

(defn flatcat [a b]
  (cond
    (and (sequential? a) (sequential? b)) (concat a b)
    (sequential? a) (conj a b)
    (sequential? b) (conj b a)
    :default [a b]))

(defn now [] (java.util.Date.))

; https://github.com/clojure/clojure/blob/clojure-1.9.0-alpha14/src/clj/clojure/core.clj#L3836
(defmacro time-mins
  "Evaluates expr and prints the time it took.  Returns the value of expr."
  [expr]
  `(let [start# (. System (nanoTime))
         ret#   ~expr
         end#   (. System (nanoTime))
         diff#  (- end# start#)
         mins-raw#   (/ (double diff#) (* 60 1000 1000000.0))
         mins#  (int mins-raw#)
         secs#  (* 60 (- mins-raw# mins#))]
     (prn (format "Elapse time: %d:%05.2f mins" mins# secs#))
          ret#))
