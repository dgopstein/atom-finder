(eq (substring-freq "aaa")  ([\a 3 ([\a 2 ([\a 1])])]))
(eq (substring-freq "aab")  ([\a 2 ([\a 1] [\b 1])] [\b 1]))
(eq (substring-freq "abc")  ([\a 1]))

(defrecord FreqNode [element count children])


(def test-node (->FreqNode \a 2 {\a (->FreqNode \a 1 {})}))
(-> test-node
    :children
    \a
    :children)



(defn inc-add
  "If a child exists, increment it, other wise add it"
  [node element]
  (update-in node [:children element] #(if (nil? (:count %1)) (->FreqNode element 1 {}) (update %1 :count inc))))

(inc-add (inc-add test-node \a) \a)

(defn substring-freq
  "Count the occurences of every substring below length m in string s"
  ([str m]
   (substring-freq (rest str) m
                   (let [root (->FreqNode (first str) 1 {})]
                     (for [lifetime (range 50)] [lifetime root]))))
   
  ([str m nodes]
   (for [char str
         node nodes]
     (:children node)
    
     )))
