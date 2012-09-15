(ns monads.identity-test
  (:use clojure.test
        monads.identity))

(deftest defmonad-test
  (testing "it creates a new map and assigns it to the given symbol"
    (defmonad test-monad
      (unit [x] x)
      (bind [m f] (f m))
      (zero 5)
      (plus [a b] (+ a b)))

    (let [expected-keys #{:unit :bind :zero :plus}
          actual-keys   (set (keys test-monad))]
      (is (= actual-keys expected-keys))
      (is (= "test" ((:bind test-monad) 'test str)))
      (is (= "yeop" ((:unit test-monad) "yeop")))
      (is (= 3      ((:plus test-monad) 1 2)))
      (is (= 5      (:zero test-monad)))))

  (testing "it accepts a doc string")
  (testing "it warns (or errors?) when unit takes the wrong number of arguments")
  (testing "it warns (or errors?) when unit is missing")
  (testing "it warns (or errors?) when bind takes the wrong number of arguments")
  (testing "it warns (or errors?) when bind is missing"))
