(ns monads.core)

(defn- parser
  "Extracts a specific element from the macro's input and returns it as a map"
  [name interpret-result]
  (letfn [(correct-form? [form] (and (coll? form) (= (symbol name) (first form))))
          (consumer [forms return]
            (cond
              (not (seq forms))             return
              (correct-form? (first forms)) (assoc return (keyword name) (interpret-result (rest (first forms))))
              :otherwise                    (recur (rest forms) return)))]
    (fn [forms] (consumer forms {(keyword name) nil}))))

(defmacro defmonad
  "Extracts all of the relevant monadic functions from the macro and binds them
   as a map to the given name"
  [name & forms]
  (letfn [(interpret-as-function [values] `(fn ~@values))
          (interpret-as-constant [values] (first values))]
     (let [parsers [(parser "plus" interpret-as-function)
                    (parser "bind" interpret-as-function)
                    (parser "unit" interpret-as-function)
                    (parser "zero" interpret-as-constant)]
          operations (apply merge (map #(% forms) parsers))]
      `(def ~name ~operations))))

(defn- make-binding [varname value remaining]
  `(~'bind ~value (fn [~varname] ~(intertwine remaining))))

(defn- intertwine [steps]
  (cond (= 1 (count steps))
          (first steps)
        (vector? (first steps))
          (let [[varname value] (first steps)]
            (make-binding varname value (rest steps)))
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

(defmacro monad [monad & body]
  (let [normalized (normalize body)
        bound-body (intertwine normalized)]
  `(let [~'bind (:bind ~monad)
         ~'unit (:unit ~monad)
         ~'zero (:zero ~monad)
         ~'plus (:plus ~monad)]
     ~bound-body)))
