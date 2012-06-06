(ns land-of-lisp.core)

(defmacro defparam
  "Mimics CLisp's defparameter to create a dynamically-bound var."
  [var val]
  `(def ~(vary-meta var assoc :dynamic true) ~val))

(defmacro setf
  "Mimics CLisp's setf function to rebind Clojure vars."
  [var val]
  `(alter-var-root #'~var (constantly ~val)))
