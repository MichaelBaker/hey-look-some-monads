(ns monads.core
  (:refer-clojure :exclude [zero?]))

(defn- zero? [form]
  (= 'zero (first form)))

(defn- find-zero [forms]
  (cond
    (not (seq forms))
      []
    (zero? (first forms))
      (vec (first forms))
    :otherwise
      (recur (rest forms))))

(defn- find-functions [forms result]
  (cond
    (not (seq forms))
      result
    (not (zero? (first forms)))
      (recur (rest forms) (conj result (first forms)))
    :otherwise
      (recur (rest forms) result)))

(def make-binding)
(def intertwine)

(defn- make-binding [varname value remaining]
  `(~'bind ~value (fn [~varname] ~(intertwine remaining))))

(defn- intertwine [steps]
  (cond (vector? (first steps))
          (let [[varname value] (first steps)]
            (make-binding varname value (rest steps)))
        (= 1 (count steps))
          (first steps)
        (= 0 (count steps))
          nil
        :otherwise
            (make-binding (gensym) (first steps) (rest steps))))

(defn- explode [bindings]
  (map vec (partition 2 bindings)))

(defn- normalize
  ([steps] (normalize steps []))
  ([steps result]
    (cond (not (seq steps))
            result
          (vector? (first steps))
            (recur (rest steps) (concat result (explode (first steps))))
          :otherwise
            (recur (rest steps) (concat result [(first steps)])))))

(defmacro defmonad [name & forms]
  (let [values    (find-zero forms)
        functions (find-functions forms [])]
    `(def ~name {:values '~values :functions '~functions})))

(defmacro monad [name & body]
  (let [normal-body (normalize body)
        bound-body  (intertwine normal-body)
        monad       (eval name)]
    `(let ~(:values monad)
      (letfn ~(:functions monad)
        ~bound-body))))

(defmacro with-monad [name & body]
  (let [monad (eval name)]
    `(let ~(:values monad)
      (letfn ~(:functions monad)
        ~@body))))
