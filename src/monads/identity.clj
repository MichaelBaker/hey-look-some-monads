(ns monads.identity)

(defn- form-parser [name interpret-result]
  (letfn [(correct-form? [form] (and (coll? form) (= (symbol name) (first form))))
          (consumer [forms return]
            (cond
              (not (seq forms))             return
              (correct-form? (first forms)) (assoc return (keyword name) (interpret-result (rest (first forms))))
              :otherwise                    (recur (rest forms) return)))]
    (fn [forms] (consumer forms {(keyword name) nil}))))

(defn- interpret-as-function [values]
  `(fn ~@values))

(defn- interpret-as-constant [values]
  (first values))

(def get-plus (form-parser "plus" interpret-as-function))
(def get-bind (form-parser "bind" interpret-as-function))
(def get-unit (form-parser "unit" interpret-as-function))
(def get-zero (form-parser "zero" interpret-as-constant))

(defmacro defmonad [name & forms]
  (let [parsers    [get-bind get-unit get-zero get-plus]
        operations (apply merge (map #(% forms) parsers))]
    `(def ~name ~operations)))

; Create a new collection of functions named "id".
; The other monad macros will use this name to look up
; the appropriate functions.
;
; This is no different than
;   (def id {:bind (fn [m f] (f m))
;            :return (fn [x] x)})
;
; Don't forget about doc strings
; (defmonad id
;   (bind [m f] (f m))
;   (return [x] x))
;
; (def id
;   {:bind   (fn [m f] (f m))
;    :return (fn [x] x)})
;
; For each pair in each vector, create a binding by
; using "bind" with the value as the first argument and
; a function encompassing the rest of the monad as the
; second argument.
;
; For each expression, evaluate it and use the result with
; the monad's "bind" but don't create a new variable binding.
; In other words, just nest invocations of "bind".
;
; The result of the monad is the result of the last
; expression.
; (monad id
;   x <- (return 5)
;   y <- (return x)
;   (return 8)
;   y)

; This is the desired result
(defn return [x] x)
(defn bind [x f] (f x))

(bind (return 5)
  (fn [x]
    (bind (return x)
      (fn [y]
        (bind (return 8)
          (fn [_]
            y))))))

; (defmacro unless  [condition & body]
;   `(if  (not ~condition)
;     (do ~@body)))
; 
; 
; (defmacro thing []
;   `(let [x 5
;          y 6]))
; 
; (macroexpand '(unless (= 1 1) (println "hello") (println "wat")))
; (macroexpand '(thing))
