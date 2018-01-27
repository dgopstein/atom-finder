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
   [clojure.math.combinatorics :as combo]
   )
(:import
   [org.eclipse.cdt.core.dom.ast gnu.cpp.GPPLanguage cpp.ICPPASTNamespaceDefinition
    IASTCompositeTypeSpecifier ASTVisitor IASTNode IASTProblemStatement IASTName
    IASTTranslationUnit IASTBinaryExpression IASTProblem IASTProblemHolder]
   [org.eclipse.cdt.core.parser DefaultLogService FileContent IncludeFileContentProvider ScannerInfo]
   [org.eclipse.cdt.internal.core.dom.parser.cpp CPPASTProblemStatement]
   [org.eclipse.cdt.internal.core.parser.scanner ASTFileLocation]
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

;; count how likely a (spot) matrix of 14 elements, with 7 domains is to have
;; X number of adjacent domains
(defn count-adjacent [a]
  (- (count a) (count (dedupe a))))

'((->> (range 8)
       (map
        #(->> (range (pap %))
              (repeat 2)
              flatten
              combo/permutations
              (map count-adjacent)
              frequencies
              (sort-by first)
              pprint
              time-mins
              ))))

;; 0
;; ([0 1])
;; "Elapse time: 0:00.03 mins"
;; 1
;; ([1 1])
;; "Elapse time: 0:00.01 mins"
;; 2
;; ([0 2] [1 2] [2 2])
;; "Elapse time: 0:00.01 mins"
;; 3
;; ([0 30] [1 36] [2 18] [3 6])
;; "Elapse time: 0:00.01 mins"
;; 4
;; ([0 864] [1 984] [2 504] [3 144] [4 24])
;; "Elapse time: 0:00.04 mins"
;; 5
;; ([0 39480] [1 43800] [2 22200] [3 6600] [4 1200] [5 120])
;; "Elapse time: 0:00.39 mins"
;; 6
;; ([0 2631600] [1 2868480] [2 1447200] [3 439200] [4 86400] [5 10800] [6 720])
;; "Elapse time: 0:20.89 mins"
;; 7
;; ([0 241133760] [1 259554960] [2 130606560] [3 40219200] [4 8290800] [5 1164240] [6 105840] [7 5040])
;; "Elapse time: 35:14.06 mins"

(def adjacency-7-domains '([0 241133760] [1 259554960] [2 130606560] [3 40219200] [4 8290800] [5 1164240] [6 105840] [7 5040]))

(float (/
 (->> adjacency-7-domains (filter (comp (partial < 4) first)) (map last) sum)
 (->> adjacency-7-domains (map last) sum)))


(->> "tmp/emacs_md5_01_0dc2e11dfd2b264679024d9939775a1ccebb13d8.c"
     parse-file
     flatten-tree count)
     find-all-atoms
     (map-values (partial map start-line))
     (map prn))

(->> "tmp/emacs_md5_02_76417ef426d826482699766064e66c06af6a07f7.c"
     parse-file
     flatten-tree count)
     flatten-tree (filter (partial instance? IASTProblem)) count)
     find-all-atoms
     (map-values (partial map start-line))
     (map prn))


'([binary-expression =]
   ([id-expression] [name x])
   ([unary-expression ++]
      ([id-expression] [name x])))

'(= (id x) (++ (id y))

'((->> "x = y++" parse-frag print-tree))
