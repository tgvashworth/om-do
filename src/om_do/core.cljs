(ns om-do.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! <! >! chan timeout alts!]]
            [clojure.string :as string]))

(enable-console-print!)

;;  # Timeline Algorithm
;;
;;  States are serialized into a vector. Each new state is pushed onto the end,
;;  and therefore has has an index.
;;
;;  Timelines are vectors where the value at each element is an index from the
;;  state vector.
;;
;;  ## Examples.
;;
;;  ### Changes on previous timeline.
;;
;;  Begin with this state. b is the 'current' slider.
;;
;;                      v
;;  0   1   2   3   4   5
;;  a0  a1  a2
;;      b0      b1  b2  b3
;;
;;  User slides b back to its 0 index. b0 is applied, which happens to be the
;;  same as a1.
;;
;;      v
;;  0   1   2   3   4   5
;;  a0  a1  a2
;;      b0      b1  b2  b3
;;
;;  User clicks on a, but doesn't change the slider position, so a is current
;;  slider. a1 may be applied, but there should be no diff.
;;
;;      v
;;  0   1   2   3   4   5
;;  a0  a1  a2
;;      b0      b1  b2  b3
;;
;;  User slides a to a2, so it is applied, the makes a change.
;;
;;                          v
;;  0   1   2   3   4   5   6
;;  a0  a1  a2              a3
;;      b0      b1  b2  b3
;;
;;  ### New timeline.
;;
;;  User has made three changes and stepped back one.
;;
;;      v
;;  0   1   2
;;  a0  a1  a2
;;
;;  User makes a new change. Append the new state to the txs list, and create the new
;;  timeline with the current state index as its zeroth element, then conj the new
;;  state index onto the new timeline.
;;
;;      v
;;  0   1   2   3
;;  a0  a1  a2
;;      b0      b1

(def todo-state
  (atom {:todos [{:text "Get lunch", :done false}
                 {:text "Eat lunch", :done false}
                 {:text "Cry into a Yorkie bar", :done false}]}))

(comment

  (fn [e]
    (let [range-val (.. e -target -value)
          state-index (current-timeline range-val)
          state-then (txs state-index)]
      (om/set-state! owner :ctlsi range-val)
      (om/transact! state nil (fn [_] state-then) :time-travel)))

)

(defn tx-timeline-view [timeline owner]
  (reify
    om/IRenderState
    (render-state [_ {:keys [timeline-index current? ctlsi time-travel-chan] :as state}]
      (let []
        (println "rendering tx-timeline-view", timeline, state)
        (apply dom/div #js {:className "tx-timeline"}
          (if current? ">" "")
          timeline-index ":"
          (map-indexed
            (fn [idx txi]
              (when-not (and (> timeline-index 0) (= idx 0))
                (dom/button
                 ;when clicked, push the timeline index and the state index onto the channel
                  #js {:onClick
                        (fn [_]
                          (when-not (= idx ctlsi)
                            (put! time-travel-chan {:timeline-index timeline-index :ctlsi idx :txi txi}))
                          (println "tx-timeline-click" timeline-index idx txi))}
                  (if (and current? (= idx ctlsi)) "*" "·"))))
            timeline))))))

(defn tx-timelines-view [app-state-cursor owner]
  (reify
    om/IInitState
    (init-state [_]
      {; states
       :txs [(om/value app-state-cursor)]
       ; timelines
       :tls [[0]]
       ; timeline index
       :tli 0
       ; current timeline state index
       :ctlsi 0
       ; channel shared with timelines
       :time-travel-chan (chan)})
    om/IWillMount
    (will-mount [_]
      (let [{:keys [tx-chan]} (om/get-shared owner)
            {:keys [time-travel-chan]} (om/get-state owner)]
        (println "tx-timelines-view WILL MOUNT")
        (go
          (loop []
            ; wait for changes
            (let [{:keys [timeline-index ctlsi txi] :as chan-data} (<! time-travel-chan)
                  {:keys [txs tls tli]} (om/get-state owner)
                  state-then (txs txi)]
              (println "~~~~~~ time travel ~~~~~~" chan-data)
              (println "state-then" state-then)
              (om/set-state! owner :tli timeline-index)
              (om/set-state! owner :ctlsi ctlsi)
              (om/transact! app-state-cursor nil (fn [_] state-then) :time-travel)
              (recur))))
        (go
          (loop []
            ; wait for changes
            (let [tx (<! tx-chan)
                  _ (println "new tx change")
                  was-time-travel? (= :time-travel (:tag tx))
                  {:keys [txs tls tli ctlsi]} (om/get-state owner)
                  next-state-index (count txs)
                  current-timeline (tls tli)
                  at-edge-of-current-timeline? (= ctlsi (- (count current-timeline) 1))]
              (println "==== change! ==============")
              (println "was-time-travel?" was-time-travel?)
              (println "tls" tls)
              (println "tli" tli)
              (println "ctlsi" ctlsi)
              (println "next-state-index" next-state-index)
              (println "at-edge-of-current-timeline?" at-edge-of-current-timeline?)
              ; conj the new state to the txs vector
              (when-not was-time-travel?
                (om/set-state! owner :txs (conj txs (:new-state tx)))
                ; if the we're at the edge of the current timeline, add the new state
                ; index to the current timeline.
                (when at-edge-of-current-timeline?
                  (om/set-state! owner [:tls tli] (conj current-timeline next-state-index)))
                ; otherwise, it's time for a new timeline!
                (when-not at-edge-of-current-timeline?
                  ; add our new timeline to the list of timelines. it contains the state index of
                  ; the current state, and the new state index
                  (om/set-state! owner :tls (conj tls [(current-timeline ctlsi) next-state-index]))
                  ; reset the current state index
                  (om/set-state! owner :ctlsi 0)
                  ; make out current timeline (tli) point to the new one
                  (om/set-state! owner :tli (count tls)))
                ; step to the next state, whatever happens
                (om/set-state! owner :ctlsi (+ (om/get-state owner :ctlsi) 1)))
              (println ".... after")
              (let [{:keys [txs tls tli ctlsi]} (om/get-state owner)]
                (println "tls" tls)
                (println "tli" tli)
                (println "ctlsi" ctlsi))
              (recur))))))
    om/IRenderState
    (render-state [_ {:keys [txs tls tli ctlsi time-travel-chan] :as tx-timelines-state}]
      (println "rendering tx-timelines-view", tls)
      (let [current-timeline (tls tli)]
        (apply dom/div #js {:className "tx-timelines"}
           (map-indexed
             (fn [idx, tl]
               (om/build tx-timeline-view tl {:state {:timeline-index idx
                                                      :ctlsi ctlsi
                                                      :current? (= idx tli)}
                                              :init-state {:time-travel-chan time-travel-chan}}))
             tls))))))

(defn todo-new-view [_ owner]
  (reify
    om/IInitState
    (init-state [_]
      {:text ""})
    om/IRenderState
    (render-state [_ {:keys [text new-chan]}]
      (dom/input #js {:className "todo-new-input"
                      :value text
                      :onChange (fn [e]
                                  (om/set-state! owner :text (.. e -target -value)))
                      :onKeyPress (fn [e]
                                    (when (== (.. e -keyCode) 13)
                                      (put! new-chan {:text text :done false})
                                      (om/set-state! owner :text "")))}))))

(defn todo-item-view [todo owner]
  (reify
    om/IRender
    (render [_]
      (dom/li #js {:className (str "todo-item" (if (:done todo) " todo-done" ""))}
        (dom/span #js {:className "todo-text"} (:text todo))
        (dom/input #js {:type "checkbox"
                        :checked (:done todo)
                        :onChange (fn [e]
                                    (println "done")
                                    (om/transact! todo :done #(.. e -target -checked)))})))))

(defn todo-view [state owner]
  (reify
    om/IInitState
    (init-state [_]
      {:new-chan (chan)})
    om/IWillMount
    (will-mount [_]
      (let [new-chan (om/get-state owner :new-chan)]
        (go
          (loop []
            (let [todo (<! new-chan)]
              (om/transact! state :todos
                #(conj % todo))
              (recur))))))
    om/IRenderState
    (render-state [_ {:keys [new-chan]}]
      (dom/div nil
        (om/build tx-timelines-view state)
        (apply dom/ul #js {:className "todo-list"}
          (om/build-all todo-item-view (:todos state)))
        (dom/div #js {:className "todo-new"}
          (om/build todo-new-view state {:state {:new-chan new-chan}}))))))

(let [tx-chan (chan)
      time-travel-chan (chan)]
  (om/root todo-view todo-state
    {:target (. js/document (getElementById "app"))
     :tx-listen (fn [tx] (println "tx-listen" tx) (put! tx-chan tx))
     :shared {:tx-chan tx-chan}}))
