(ns shadow.arborist
  {:doc "Arborists generally focus on the health and safety of individual plants and trees."
   :definition "https://en.wikipedia.org/wiki/Arborist"}
  (:require-macros
    [shadow.arborist]
    [shadow.arborist.fragments])
  (:require
    [shadow.arborist.protocols :as p]
    [shadow.arborist.fragments :as frag]
    [shadow.arborist.attributes :as attr]
    [shadow.arborist.components :as comp]
    [shadow.arborist.common :as common]
    [shadow.arborist.collections :as coll]))

(deftype TreeScheduler [root ^:mutable update-pending?]
  p/IScheduleUpdates
  (schedule-update! [this component]
    (set! update-pending? true))

  (schedule-event! [this event-fn]
    (event-fn)
    (when update-pending?
      (set! update-pending? false)
      (p/sync! root)
      (js/console.log "sync complete!" update-pending?))))

(deftype TreeRoot [container ^:mutable env ^:mutable root]
  p/ITraverseNodes
  (managed-nodes [this]
    (p/managed-nodes root))

  p/IDirectUpdate
  (update! [this next]
    (if root
      (p/update! root next)
      (let [new-root (common/ManagedRoot. env nil nil)]
        (set! root new-root)
        (p/update! root next)
        (p/dom-insert root container nil)
        )))

  p/ITreeNode
  (sync! [this]
    (p/sync! root))

  p/IDestructible
  (destroy! [this]
    (when root
      (p/destroy! root))))

(defn dom-root
  ([container env]
   (let [root (TreeRoot. container nil nil)
         scheduler (TreeScheduler. root false)
         root-env (assoc env ::root root ::comp/scheduler scheduler)]
     (set! (.-env root) root-env)
     root))
  ([container env init]
   (doto (dom-root container env)
     (p/update! init))))

(defn >>
  ([component props]
   {:pre [(or (map? component) (fn? component))
          (map? props)]}
   (comp/ComponentNode. component props))
  ;; FIXME: this works but shouldn't be used directly?
  #_([component props & children]
      (comp/ChildrenNode.
        (>> component props)
        (vec children))))

(defn << [& body]
  (throw (ex-info "<< can only be called used a macro" {})))

(defn fragment [& body]
  (throw (ex-info "fragment can only be called used a macro" {})))

(defn slot [env]
  (comp/slot env))

;; FIXME: to be ->> friendly this should take coll last?
(defn render-seq [coll key-fn render-fn]
  (coll/node coll key-fn render-fn))

(defn update! [x next]
  (p/update! x next))

(defn destroy! [root]
  (p/destroy! root))

js->clj