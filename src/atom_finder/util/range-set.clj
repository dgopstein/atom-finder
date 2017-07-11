(in-ns 'atom-finder.util)
(import '(com.google.common.collect Range ImmutableRangeSet))

(s/defn range-set
  "Take multiple disjoint ranges and construct a function which
    determines whether a number is included in any of the ranges"
  [ranges :- [[(s/one s/Int "min") (s/one s/Int "max")]]]
  (let [builder (ImmutableRangeSet/builder)
        imr (do (doseq [[a b] ranges]
                  (.add builder (Range/closedOpen a b)))
                (.build builder))]

  #(.contains imr %)))
