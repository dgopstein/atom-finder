;; find parens which are not technically necessary

(ns atom-finder.questions.superfluous-parens
  (:require
   [atom-finder.constants :refer :all]
   [atom-finder.classifier :refer :all]
   [atom-finder.util :refer :all]
   [atom-finder.tree-diff :refer :all]
   [atom-finder.questions.question-util :refer :all]
   [clj-cdt.clj-cdt :refer :all]
   [clj-cdt.expr-operator :refer :all]
   [clj-cdt.writer-util :refer :all]
   [clojure.pprint :refer [pprint]]
   [clojure.string :as string]
   [swiss.arrows :refer :all]
   [schema.core :as s]
   ))


(s/defn superfluous-parens? [node :- (s/pred paren-node?)]
  (let [mom (parent node)
        kid (child node)
        parenless-mom (replace-expr mom node kid)
        reparsed-parenless-mom (->> parenless-mom write-ast parse-expr)
        ]

    (tree=by (juxt class expr-operator) (pap print-tree parenless-mom) (pap print-tree reparsed-parenless-mom))
  ))
