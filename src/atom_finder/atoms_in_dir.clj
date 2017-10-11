(ns atom-finder.atoms-in-dir
  (:require [atom-finder.util :refer :all]
            [atom-finder.classifier :refer :all]
            [atom-finder.constants :refer :all]
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
     root-ancestor .getMacroExpansions first
     .getNestedMacroReferences (into []))
     ppublic-methods)

(->> "
  #define ltq_w32(x) x
  #define ltq_pci_w32(x)	ltq_w32((x))
  #define ltq_pci_r32(x)		3
	ltq_pci_w32(ltq_pci_r32(1) + 2);
"
     (tap (fn [x] (println " ")))
     parse-source
     root-ancestor .getMacroExpansions first
     inner-macro-operator-atom?)

;; [^(])

  (->> "~/opt/src/linux"
       expand-home
       (pmap-dir-files #(atoms-in-file (map atom-lookup [:macro-operator-precedence]) %))
       (filter (fn [[file atms]] (->> atms vals (exists? (complement empty?)))))
       (map prn)
       count
       println
       time-mins)
