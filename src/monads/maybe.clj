(ns monads.maybe
  (:use monads.core))

(defmonad m-maybe
  (unit [x] x)
  (bind [m f]
    (if (= m nil)
      nil
      (f m)))
  (zero nil))
