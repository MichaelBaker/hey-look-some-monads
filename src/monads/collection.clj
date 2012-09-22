(ns monads.collection
  (:use monads.core
        clojure.pprint))

(defmonad m-collection
  (collection [& args] args)
  (unit [x] [x])
  (bind [m f]
    (apply concat (map f m)))
  (zero []))
