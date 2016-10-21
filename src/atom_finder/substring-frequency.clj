(= (subs-freq "aaa" 3)  ([\a 3 ([\a 2 ([\a 1])])]))
(= (subs-freq "aab" 3)  ([\a 2 ([\a 1] [\b 1])] [\b 1]))
(= (subs-freq "abc" 3)  ([\a 1]))

(defrecord FreqNode [element count children])


(def test-node (->FreqNode \a 2 {\a (->FreqNode \a 1 {})}))
(-> test-node
    :children
    (get \a)
    :children)



(defn inc-add
  "If a child exists, increment it, otherwise add it"
  [node element]
  (update-in node [:children element]
             #(if (nil? (:count %1))
                (->FreqNode element 1 {})
                (update %1 :count inc))))

(inc-add (inc-add test-node \a) \a)

;; (defn substring-freq
;;   "Count the occurences of every substring below length m in string s"
;;   ([str m]
;;    (substring-freq (rest str) m
;;                    (let [root (->FreqNode (first str) 1 {})]
;;                      (for [lifetime (range 50)] [lifetime root]))))
   
;;   ([str m nodes]
;;    (let [char (first str)]
;;      (for [node nodes]
;;        (inc-add node (:children node))

;;        )
       
    
;;      ))))


(defn all-subs [str m]
  (let [str-len (count str)]
    (for [start (range 0 str-len)
          end   (range (+ start 1) (min (+ start m 1) str-len))]
      (do
        (prn [start end])
        (subs str start end)))))

(defn subs-freq [str max-len]
  (frequencies (all-subs str max-len)))

(subs-freq "aabc" 8)
