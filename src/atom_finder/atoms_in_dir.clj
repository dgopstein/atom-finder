(ns atom-finder.atoms-in-dir
  (:require [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.constants :refer :all]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            ))

(defn all-atoms-in-tree
  "Find all instances of multiple atoms under this node"
  [atoms node]
  (->>
   atoms
   (map #(vector
          (:name %1)
          ;(:finder %1)))
          ((:finder %1) node)))
   (into {})
   ))

(defn atoms-in-file
  [atoms filename]
  [filename
   (->> filename
        expand-home
        parse-file
        (all-atoms-in-tree atoms)
       (map-values (partial map (comp :line loc)))
        )])

(defn print-atoms-in-dir
  [dirname atoms]
  (->> dirname
       (pmap-dir-files #(atoms-in-file atoms %))
       (map prn)
       count
       println
       time-mins
       ))

; (print-atoms-in-dir (expand-home "~/opt/src/redis") (map atom-lookup [:preprocessor-in-statement :reversed-subscript]))
; (print-atoms-in-dir (expand-home "~/opt/src/linux") (map atom-lookup [:macro-operator-precedence]))

(require '[atom-finder.classifier :refer :all])
(->> "x->y" parse-frag print-tree)
(->> "x.y" parse-frag print-tree)
(->> "#define N	x
      #define M(x)  N(x)
      int y = M();"
     parse-frag
     root-ancestor .getMacroExpansions first write-tree)

(->> "
  #define M(x) f(x)
  M((u32)action);
"
     (tap (fn [x] (println " ")))
     parse-frag
     root-ancestor .getMacroExpansions first
     inner-macro-operator-atom?)



;; [^(])

  (->> "~/opt/src/linux"
       expand-home
       (pmap-dir-files #(atoms-in-file (map atom-lookup [:literal-encoding]) %))
       (filter (fn [[file atms]] (->> atms vals (exists? (complement empty?)))))
       (map prn)
       count
       println
       time-mins)

(->> "~/opt/src/linux"
       expand-home
       (pmap-dir-trees (->> atom-lookup :literal-encoding :finder))
       flatten
       (take 1000)
       (map write-tree)
       (map println)
       )

(->> "~(3 | 2)" parse-frag print-tree)
(->> "~ 3 | 2" parse-frag print-tree)

(->> "~/opt/src/linux"
     expand-home
     (pmap-dir-files identity)
     (take 5)
     pprint)

(->> "/Users/dgopstein/opt/src/linux/arch/alpha/boot/main.c"
     parse-file
     print-tree)
