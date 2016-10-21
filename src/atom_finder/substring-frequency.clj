(defn all-subs [str m]
  (let [str-len (count str)]
    (for [start (range 0 str-len)
          end   (range (+ start 1) (min (+ start m 1) (+ str-len 1)))]
      (do
        (prn [start end])
        (subs str start end)))))

(defn subs-freq [str max-len]
  (into (sorted-map) (frequencies (all-subs str max-len))))

(subs-freq "aabc" 8)
