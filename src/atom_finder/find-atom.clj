(defmulti side-effecting? class)
(defmethod side-effecting? :default [node] false)
(defmethod side-effecting? :default [node] false)

(defn contains-location?
  "Does this node contain the given offset/length"
  [root offset length]
  (let [root-loc (.getFileLocation root)
        root-offset (.getNodeOffset root-loc)
        root-length (.getNodeLength root-loc)]

    ;; The location/offset is fully contained in this node
    (and (<=    root-offset                 offset)
         (>= (+ root-offset root-length) (+ offset length)))))

(defn location-parent
  "Find the AST node that contains the whole location offset/length
   Assumes that no children of a single parent overlap in terms of offset/location"
  [root offset length]
  (let [kids      (children root)
        container (first (filter #(contains-location? % offset length) kids))]
    (if (nil? container)
      root
      (recur container offset length))))

(defn macro-in-expressions
  "return a list of all macros that are defined inside of expressiosn"
  [root]
  (for [md (.getMacroDefinitions root)]
    (let [offset (.getNodeOffset (.getExpansionLocation md))
         length  (.getNodeLength (.getExpansionLocation md))
          in-expr?  (instance? IASTExpression (location-parent root offset length))]

      [md in-expr?]
      )))

(for [[md in-expr?] (macro-in-expressions root)]
  [(.getRawSignature md) in-expr?])


