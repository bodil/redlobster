(ns redlobster.events)

;; Protocol

(defprotocol IEventEmitter
  (on [emitter event listener])
  (once [emitter event listener])
  (remove-listener [emitter event listener])
  (remove-all-listeners [emitter]
                        [emitter event])
  (listeners [emitter event])
  (emit [emitter event data]))

;; Utility functions

(defn unpack-event [event]
  (if (keyword? event)
    (name event)
    event))

(defn wrap-once [emitter event listener]
  (fn once-off [x]
    (listener x)
    (remove-listener emitter event once-off)))

;; Default implementation

(defn- def-add-listener [type listener]
  (fn [this]
    (let [listeners (or (get this type) #{})]
      (assoc this type (conj listeners listener)))))

(defn- def-rem-listener [type listener]
  (fn [this]
    (let [listeners (or (get this type) #{})]
      (assoc this type (disj listeners listener)))))

(deftype DefaultEventEmitter [events]
  IEventEmitter
  (on [this event listener]
    (swap! events (def-add-listener event listener)))
  (once [this event listener]
    (set! (.-__redlobster_event_once listener) true)
    (swap! events (def-add-listener event listener)))
  (remove-listener [this event listener]
    (swap! events (def-rem-listener event listener)))
  (remove-all-listeners [this]
    (reset! events {}))
  (remove-all-listeners [this event]
    (swap! events #(dissoc % event)))
  (listeners [this event]
    (get @events event))
  (emit [this event data]
    (doseq [listener (listeners this event)]
      (listener data)
      (when (.-__redlobster_event_once listener)
        (remove-listener this event listener)))))

;; Implementations

(def ^:private implementations
  [(fn impl-node []
     (try
       (let [EventEmitter (.-EventEmitter (js/require "events"))]
         {:constructor (fn [] (EventEmitter.))
          :type :node
          :init
          (fn []
            (extend-protocol IEventEmitter
              EventEmitter
              (on [emitter event listener]
                (.on emitter (unpack-event event) listener))
              (once [emitter event listener]
                (.once emitter (unpack-event event) listener))
              (remove-listener [emitter event listener]
                (.removeListener emitter (unpack-event event) listener))
              (remove-all-listeners 
                ([emitter]
                  (.removeAllListeners emitter))
                ([emitter event]
                  (.removeAllListeners emitter (unpack-event event))))
              (listeners [emitter event]
                (js->clj (.listeners emitter (unpack-event event))))
              (emit [emitter event data]
                (.emit emitter (unpack-event event) data))))})
       (catch js/Error e nil)))

   (fn impl-default []
     {:constructor (fn [] (DefaultEventEmitter. (atom {})))
      :type :default
      :init (fn [])})])

;; Initialise the first available implementation

(let [emitter (some #(%) implementations)]
  (if (nil? emitter) (throw (js/Error. "No supported EventEmitter found"))
      (do
        (def event-emitter (:constructor emitter))
        (def emitter-type (:type emitter))
        ((:init emitter)))))
