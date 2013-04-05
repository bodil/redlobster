(ns redlobster.promise-test
  (:use-macros [redlobster.macros :only [promise]])
  (:require [redlobster.promise :as p]))

(def test (atom 2))

(let [promise (p/promise)]
  (p/on-realised promise
                 (fn [v] (swap! test #(+ % v)))
                 (fn [_] (assert false)))
  (p/realise promise 3)
  (assert (= @test 5)
          "on-realised success listener should be called with realised value"))

(reset! test 2)
(let [promise (p/promise)]
  (p/on-realised promise
                 (fn [_] (assert false))
                 (fn [v] (swap! test #(+ % v))))
  (p/realise-error promise 2)
  (assert (= @test 4)
          "on-realised error listener should be called with error value"))

(let [promise (p/promise)
      chained (p/chain promise count)]
  (assert (= @promise :redlobster.promise/not-realised)
          "unrealised promise should resolve to :not-realised")
  (assert (not (p/realised? promise))
          "realised? should return false for unrealised promises")
  (assert (not (p/failed? promise))
          "failed? should return false for unrealised promises")
  (assert (not (p/realised? chained))
          "realised? should return false for unrealised chained promises")
  (p/realise promise "ohai")
  (assert (p/realised? promise)
          "realised? should return true for realised promises")
  (assert (not (p/failed? promise))
          "failed? should return false for successful promises")
  (assert (= @promise "ohai")
          "realised promise should deref to realised value")
  (assert (p/realised? chained)
          "realised? should return true for realised chained promises")
  (assert (= @chained 4)
          "realised chained promise should deref to realised value"))

(let [promise (p/promise)
      chained (p/chain promise count)]
  (p/realise-error promise "boom")
  (assert (p/realised? promise)
          "realised? should return true for realised promises")
  (assert (p/failed? promise)
          "failed? should return true for failed promises")
  (assert (= @promise "boom")
          "failed promise should deref to error value")
  (assert (p/failed? chained)
          "failed? on a chained promise should pass through")
  (assert (= @chained "boom")
          "failed promise chain should deref to error value")
  )

