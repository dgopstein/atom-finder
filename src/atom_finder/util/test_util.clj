(in-ns 'atom-finder.util)
(require '[clojure.test :refer :all])

(defn find-lines
  "Find all lines marked with <true> in test file"
  [pat filepath]
  (let [regex (re-pattern pat)]
    (->>
     filepath
     slurp-lines
     (map #(re-find regex %))
     (keep-indexed #(if %2 (inc %1))))))

(def true-lines (partial find-lines "<true>"))

(defmacro test-atom-lines
  "Compare the results of an atom classifier with a regex search"
  [filename pat atom-finder]
  `(let [filepath#   (resource-path ~filename)
         expected#   (find-lines ~pat filepath#)
         lines#  (->> filepath#
                      parse-file
                      (~atom-finder)
                      (map loc)
                      (map :line))]

    (is (empty? (sort (sym-diff (set expected#) (set lines#)))))))

(defn parse-resource
  "Parse a file in the resource directory"
  [filename]
  (->> filename resource-path parse-file))
