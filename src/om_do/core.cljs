(ns om-do.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs.core.async :refer [put! <! >! chan timeout alts!]]))

(enable-console-print!)

(def todo-state
  (atom {:todos [{:text "Get lunch", :done false}
                 {:text "Eat lunch", :done false}
                 {:text "Cry into a Yorkie bar", :done false}]}))

(defn todo-new-view [_ owner]
  (reify
    om/IInitState
    (init-state [_]
      {:text ""})
    om/IRenderState
    (render-state [_ {:keys [text new]}]
      (dom/input #js {:className "todo-new-input"
                      :value text
                      :onChange (fn [e]
                                  (om/set-state! owner :text (.. e -target -value)))
                      :onKeyPress (fn [e]
                                    (when (== (.. e -keyCode) 13)
                                      (put! new {:text text :done false})
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
                                    (om/transact! todo :done #(.. e -target -checked)))})))))

(defn tx-timeline-view [state owner]
  (reify
    om/IInitState
    (init-state [_]
      {:txs [state]
       :ctx 0})
    om/IWillMount
    (will-mount [_]
      (let [{:keys [tx-chan]} (om/get-shared owner)]
        (go
          (loop []
            (let [tx (<! tx-chan)
                  was-time-travel (= :time-travel (:tag tx))
                  {:keys [txs ctx]} (om/get-state owner)]
              (when-not was-time-travel
                (om/set-state! owner :txs (conj txs (:new-state tx))))
              (when (= ctx (- (count txs) 1))
                (om/set-state! owner :ctx (+ ctx 1)))
              (recur))))))
    om/IRenderState
    (render-state [_ {:keys [txs ctx] :as timeline-state}]
      (dom/input #js {:type "range"
                      :min 0
                      :max (- (count txs) 1)
                      :value ctx
                      :step 1
                      :onChange
                        (fn [e]
                          (let [range-val (.. e -target -value)
                                state-then (txs range-val)]
                            (om/set-state! owner :ctx range-val)
                            (om/transact! state nil (fn [_] state-then) :time-travel)))}))))

(defn todo-view [state owner]
  (reify
    om/IInitState
    (init-state [_]
      {:new (chan)})
    om/IWillMount
    (will-mount [_]
      (let [new (om/get-state owner :new)]
        (go
          (loop []
            (let [todo (<! new)]
              (om/transact! state :todos
                #(conj % todo))
              (recur))))))
    om/IRenderState
    (render-state [_ {:keys [new]}]
      (dom/div nil
        (om/build tx-timeline-view state)
        (apply dom/ul #js {:className "todo-list"}
          (om/build-all todo-item-view (:todos state)))
        (dom/div #js {:className "todo-new"}
          (om/build todo-new-view state {:state {:new new}}))))))

(let [tx-chan (chan)]
  (om/root todo-view todo-state
    {:target (. js/document (getElementById "app"))
     :tx-listen #(put! tx-chan %)
     :shared {:tx-chan tx-chan}}))
