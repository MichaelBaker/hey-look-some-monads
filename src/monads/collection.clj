(ns monads.collection
  (:use monads.core))

(defmonad m-collection
  (unit [x]
    [x])

  (bind [m f]
    (apply concat (map f m)))

  (collection [& args]
    args)

  (zero []))