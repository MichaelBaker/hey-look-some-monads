(ns monads.state
  (:use monads.core))

(defmonad m-state
  (unit [x]
    (fn [state] [state x]))

  (bind [m f]
    (fn [start-state]
      (let [[new-state value] (m start-state)]
        ((f value) new-state))))

  (get-state []
    (fn [state] [state state]))

  (set-state [state]
    (fn [_] [state nil]))

  (modify-state [f]
    (fn [state] [(f state) nil])))
