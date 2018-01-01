(ns atom-finder.atom-stats
  (:require
   [atom-finder.util :refer :all]
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.atom-stats :refer :all]
   [atom-finder.atoms-in-dir :refer :all]
   [clojure.pprint :refer [pprint]]
   [schema.core :as s]
   [swiss.arrows :refer :all]
   )
  )

(defn file-level-key? [key] (not (re-find #"atom" (str key))))

(def keep-numeric-vals (partial dissoc-by (comp not number? last)))

(s/defn sum-atom-counts
  [n-atoms :- s/Int map-list]; :- [{:ast-size-before s/Int s/Any s/Any}]]
  (->> map-list
       (map keep-numeric-vals)
       (apply merge-with +)
       ;(map-values-kv (fn [k v] (if (file-level-key? k) (long (/ v n-atoms)) v)))
       ))

(defn stats-by-file-type
  [flat-data]
  (let [n-atoms (->> flat-data (take 100) (map :atom) distinct (remove nil?) count)]
    (->> flat-data
         (group-by (comp file-ext :file))
         (map-values (partial sum-atom-counts n-atoms))
         ))
  )

;; Exhaustive list of files containing a Reversed Subscript atom
(def reversed-subscript-files [
  "freebsd/crypto/heimdal/appl/ftp/ftp/security.c"
  "freebsd/crypto/heimdal/appl/ftp/ftpd/security.c"
  "vim/runtime/macros/maze/mazeclean.c"
  "webkit/Source/ThirdParty/libwebrtc/Source/third_party/protobuf/php/ext/google/protobuf/upb.c"
  "webkit/Source/ThirdParty/libwebrtc/Source/third_party/protobuf/ruby/ext/google/protobuf_c/upb.c"
  "gcc/gcc/testsuite/gcc.dg/Wchar-subscripts-1.c"
  "gcc/gcc/testsuite/gcc.dg/array-8.c"
  "gcc/gcc/testsuite/gcc.dg/pointer-arith-3.c"
  "gcc/gcc/testsuite/gcc.dg/pointer-arith-1.c"
  "gcc/gcc/testsuite/gcc.dg/pointer-arith-4.c"
  "gcc/gcc/testsuite/gcc.dg/torture/pr70421.c"
  "gcc/gcc/testsuite/gcc.dg/pointer-arith-2.c"
  "gcc/gcc/testsuite/gcc.target/i386/pr70662.c"
  "gcc/gcc/testsuite/g++.old-deja/g++.bugs/900321_05.C"
  "gcc/gcc/testsuite/g++.dg/cpp0x/constexpr-nullptr-2.C"
  "gcc/gcc/testsuite/g++.dg/conversion/simd4.C"
  "gcc/gcc/testsuite/gcc.c-torture/execute/pr22061-1.c"
  "clang/test/Sema/c89.c"
  "clang/test/Sema/warn-char-subscripts.c"
  "clang/test/Profile/c-general.c"
  "clang/test/CodeGen/2003-07-22-ArrayAccessTypeSafety.c"
  "clang/test/CodeGenCXX/temporaries.cpp"
  "clang/test/CodeGenCXX/cxx1z-eval-order.cpp"
  "clang/test/CodeGenCXX/catch-undef-behavior.cpp"
  "clang/test/CodeGenCXX/typeid-should-throw.cpp"
  "clang/test/Parser/cxx-bool.cpp"
  "clang/test/SemaCXX/overloaded-builtin-operators.cpp"
  "clang/test/SemaCXX/return-stack-addr.cpp"
  "clang/test/SemaCXX/typo-correction.cpp"])

'((->> reversed-subscript-files
     (map (partial str "~/opt/src/atom-finder/"))
     (map parse-file)
     (mapcat (->> atom-lookup :reversed-subscript :finder))
     (map (juxt filename start-line))
     (map prn)
     ))

'((->> "webkit/Source/ThirdParty/libwebrtc/Source/third_party/protobuf/php/ext/google/protobuf/upb.c"
     (str "~/opt/src/atom-finder/")
     parse-file
     (filter-tree (->> atom-lookup :reversed-subscript :classifier))
     (map start-line)
     ))


/* Create a power-of-two histogram of the table keys. */
int counts[UPB_MAXARRSIZE + 1] = {0};
ARRAY_SIZE(counts)

#define ARRAY_SIZE(x) \
  ((sizeof(x)/sizeof(0[x])) / ((size_t)(!(sizeof(x) % sizeof(0[x])))))
