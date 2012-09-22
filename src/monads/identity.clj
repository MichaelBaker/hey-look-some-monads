(ns monads.identity
  (:use monads.core))

(defmonad m-identity
  (bind [m f]
    (f m))

  (unit [x]
    x))
