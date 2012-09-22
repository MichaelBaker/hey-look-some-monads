(ns monads.maybe
  (:use monads.core))

(defmonad m-maybe
  (unit [x] (with-meta {:value x} {:m-maybe :just}))
  (bind [m f]
    (if (= :nothing (:m-maybe (meta m)))
      (:value zero)
      (f (:value m))))
  (zero (with-meta {:value nil} {:m-maybe :nothing})))
