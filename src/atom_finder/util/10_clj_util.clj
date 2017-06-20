(in-ns 'atom-finder.util)

(defmacro %w [& words]
    `(list ~@(map str (vec words))))

(defn tap [f x] (f x) x)
(defn pap
  "print the value of the argument and return it; optionally modified by a function"
  ([x] (tap prn x))
  ([f x] (tap (comp prn f) x)))

(defmacro =by
  "Test if arguments are equal after applying f to all of them"
  [f & args]
  `(= ~@(map #(list f %) args)))

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
  (try (/ x y)
       (catch ArithmeticException _
         ;(println "Division by zero caught!")
         (cond (> x 0)   Double/POSITIVE_INFINITY
               (zero? x) Double/NaN
               :else     Double/NEGATIVE_INFINITY))))

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
