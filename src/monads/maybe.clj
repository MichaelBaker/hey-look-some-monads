(ns monads.maybe
  (:use monads.core))

(defn- nothing? [m]
  (= :nothing (:m-maybe (meta m))))

(defmonad m-maybe
  (unit [x]
    (with-meta {:value x} {:m-maybe :just}))

  (bind [m f]
    (if (nothing? m)
      (:value zero)
      (f (:value m))))

  (plus [x y]
    (if (nothing? x)
      y
      x))

  (zero (with-meta {:value nil} {:m-maybe :nothing})))
