(in-ns 'atom-finder.util)
(import '(java.lang.instrument Instrumentation))

(defn heap-size [] (.totalMemory (Runtime/getRuntime)))
(defn free-memory [] (.freeMemory (Runtime/getRuntime)))
(defn used-memory [] (long (/ (- (heap-size) (free-memory)) (* 1024 1024))))

;(defn object-size [o] (Instrumentation/getObjectSize o))

;; print methods of java object
;; http://stackoverflow.com/questions/5821286/how-can-i-get-the-methods-of-a-java-class-from-clojure
(defn java-methods
  "list methods of java object"
  [obj]
  (->> obj
       rflct/reflect
       (:members)
       ;(filter :exception-types)
       (map #(dissoc % :exception-types))
       (map #(dissoc % :declaring-class))
       (sort-by :name)
       ))

(defn public-methods
  "list public methods of java object"
  [obj]
  (->> obj
       (java-methods)
       (filter #(:public (:flags %)))
       ))

(defn ppublic-methods
  "print public methods of java object"
  [obj]
  (->> obj
       (public-methods)
       (print-table)
       ))

; https://gist.github.com/sunng87/13700d3356d5514d35ad
(defn invoke-private-method [obj fn-name-string & args]
  (let [m (first (filter (fn [x] (.. x getName (equals fn-name-string)))
                         (.. obj getClass getDeclaredMethods)))]
    (. m (setAccessible true))
    (. m (invoke obj args))))

(defn private-field [obj fn-name-string]
  (let [m (.. obj getClass (getDeclaredField fn-name-string))]
    (. m (setAccessible true))
        (. m (get obj))))

(defn write-tempfile
  [content]
  ; https://github.com/clojure-cookbook/clojure-cookbook/blob/master/04_local-io/4-10_using-temp-files.asciidoc
  (let [my-temp-file (java.io.File/createTempFile "filename" ".txt")]
    (with-open [file (clojure.java.io/writer my-temp-file)]
      (binding [*out* file]
        (print content)))

    my-temp-file))
