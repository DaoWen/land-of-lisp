(ns land-of-lisp.core)

(defmacro defparam [var val]
  "Mimics CLisp's defparameter to create a dynamically-bound var."
  `(def ~(vary-meta var assoc :dynamic true) ~val))

(defmacro setf [var val]
  "Mimics CLisp's setf function to rebind Clojure vars."
  `(alter-var-root #'~var (constantly ~val)))
