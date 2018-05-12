(in-ns 'atom-finder.util)

(require '[clojail.core :refer [thunk-timeout]])

(defmacro log-timeout [time msg & body]
  `(try (thunk-timeout (fn [] ~@body) ~time :seconds)
        (catch java.util.concurrent.TimeoutException e#
          (do
            (errln (str "[Timeout " ~time "s] " ~msg))
            nil))))

(defmacro with-timeout [time & body]
  `(log-timeout ~time (str "body: " '~@body) ~@body))

(defmacro with-timeout-ms [time & body]
  `(try (thunk-timeout (fn [] ~@body) ~time :ms)
        (catch java.util.concurrent.TimeoutException e#
          (do
            (errln (str "Killed operation when it exceded max duration of " ~time "ms, body: " '~@body))
            nil))))
