(ns redlobster.promise
  (:require-macros [cljs.node-macros :as n])
  (:require [redlobster.events :as e]))

(defprotocol IPromise
  (realised? [this])
  (failed? [this])
  (realise [this value])
  (realise-error [this value])
  (on-realised [this on-success on-error]))

(defn promise? [v]
  (satisfies? IPromise v))

(deftype Promise [ee source]
  IDeref
  (-deref [this]
    (let [realised (.-__realised ee)
          value (.-__value ee)]
      (cond
       (not realised) :redlobster.promise/not-realised
       :else value)))
  IPromise
  (realised? [this]
    (if (nil? (.-__realised ee)) false true))
  (failed? [this]
    (and (realised? this) (= :error (.-__realised ee))))
  (realise [this value]
    (if (realised? this)
      (when-not (= :redlobster.promise/timeout @this)
        (throw :redlobster.promise/already-realised))
      (if (promise? value)
        (on-realised value
                     #(realise this %)
                     #(realise-error this %))
        (do
          (set! (.-__realised ee) :success)
          (set! (.-__value ee) value)
          (e/emit ee :realise-success value)))))
  (realise-error [this value]
    (if (realised? this)
      (when-not (= :redlobster.promise/timeout @this)
        (throw :redlobster.promise/already-realised))
      (if (promise? value)
        (on-realised value
                     #(realise this %)
                     #(realise-error this %))
        (do
          (set! (.-__realised ee) :error)
          (set! (.-__value ee) value)
          (e/emit ee :realise-error value)))))
  (on-realised [this on-success on-error]
    (if (realised? this)
      (if (failed? this) (on-error @this) (on-success @this))
      (doto ee
        (e/on :realise-success on-success)
        (e/on :realise-error on-error)))))

(defn sourced-promise
  ([source]
     (Promise.
       (let [ee (e/event-emitter)]
         (set! (.-__realised ee) nil)
         (set! (.-__value ee) nil)
         ee)
       source))
  ([source success-value]
     (doto (sourced-promise source)
       (realise success-value))))

(defn promise
  ([]
   (sourced-promise nil))
  ([success-value]
   (sourced-promise nil success-value)))

(defn promise-fail [error-value]
  (doto (promise)
    (realise-error error-value)))

(defn chain [this f]
  (let [p (sourced-promise this)]
    (on-realised this
                 #(realise p (f %))
                 #(realise-error p %))
    p))

(defn await
  "Takes a list of promises, and creates a promise that will realise as
`:redlobster.promise/realised` when each promise has successfully realised,
or if one or more of the promises fail, fail with the value of the first
failing promise.

If the first argument is the keyword `:all`, then instead of failing when
one of the promises fails, it will just wait for all promises to realise
and realise itself with `:redlobster.promise/realised` regardless of the
success or failure of any promise."
  [& promises]
  (let [await-all (= (first promises) :all)
        promises (if await-all (rest promises) promises)
        p (sourced-promise promises)
        total (count promises)
        count (atom 0)
        done (atom false)]
    (doseq [subp promises]
      (let [succ (fn [_]
                   (when (not @done)
                     (swap! count inc)
                     (when (= total @count)
                       (reset! done true)
                       (realise p :redlobster.promise/realised))))
            fail (if await-all succ
                     (fn [err]
                       (when (not @done)
                         (reset! done true)
                         (realise-error p err))))]
        (on-realised subp succ fail)))
    p))

(defn defer-until-realised [promises callback]
  (let [p (sourced-promise promises)]
    (on-realised (apply await promises)
                 (fn [_] (realise p (callback)))
                 (fn [error] (realise-error p error)))
    p))

(defn on-event
  "Creates a promise that fulfills with an event object when the matching
event is triggered on the EventEmitter. This promise cannot fail; it will
either succeed or never realise."
  [ee type]
  (let [p (promise)]
    (e/once ee type
          (fn [event] (realise p event)))
    p))

(defn timeout
  "Sets a promise to fail with `:redlobster.promise/timeout` after a
specified number of milliseconds.

A promise that has timed out will not throw an error when you try to
realise it, but the realised value will remain
`:redlobster.promise/timeout`."
  [promise timeout]
  (let [timeout-func #(when-not (realised? promise)
                        (realise-error promise :redlobster.promise/timeout))]
    (js/setTimeout timeout-func timeout)))
