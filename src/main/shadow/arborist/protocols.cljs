(ns shadow.arborist.protocols)

(defprotocol IControlFragment
  (fragment-build [this env vals] "returns fragment state, required for update")
  (fragment-update [this env root nodes ovals nvals]))

(defprotocol IConstruct
  (as-managed [this env]))

(defprotocol ITreeNode
  ;; perform all required node updates, call sync! on all managed nodes
  ;; FIXME: calling sync! recursively is bad as stack may get deep
  ;; should use different method of tree traversal, maybe something like react fiber
  (sync! [this]))

(defprotocol IManageNodes
  (dom-insert [this parent anchor])
  (dom-first [this]))

(defprotocol ITraverseNodes
  (managed-nodes [this]))

(defprotocol IUpdatable
  (supports? [this next])
  (dom-sync! [this next]))

(defprotocol IHaveSlots
  (dom-slot [this id]))

(defprotocol IAmStateful
  (invalidate! [this]))

(defprotocol IDestructible
  (destroyed? [this])
  (destroy! [this]))

(defprotocol IHandleEvents
  (handle-event! [this ev-id e ev-args]))

;; root user api
(defprotocol IDirectUpdate
  (update! [this next]))

(defprotocol IScheduleUpdates
  (schedule-update! [this target])
  (schedule-event! [this event-fn]))

(defmulti set-attr*
  (fn [env node key oval nval] key)
  :default ::default)