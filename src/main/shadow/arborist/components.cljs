(ns shadow.arborist.components
  (:require
    [goog.object :as gobj]
    [shadow.arborist.common :as common]
    [shadow.arborist.protocols :as p]))

(declare component-node?)

(defn safe-inc [x]
  (if (nil? x)
    0
    (inc x)))

(defonce component-id-seq (atom 0))

(defn next-component-id []
  (swap! component-id-seq inc))

(set! *warn-on-infer* false)

(deftype ManagedComponent
  [parent-env
   config
   ^:mutable props
   ^:mutable ^not-native root ;; must not be modified after init
   ^:mutable component-env ;; must not be modified after init
   ^:mutable render-fn
   ^:mutable state ;; FIXME: what if there was no state?
   ^:mutable dirty?
   ^:mutable destroyed?
   slots ;; map of {slot-id target-node}
   ]

  p/IManageNodes
  (dom-first [this]
    (p/dom-first root))

  (dom-insert [this parent anchor]
    (p/dom-insert root parent anchor))

  ;; FIXME: this could just delegate to the root?
  p/ITraverseNodes
  (managed-nodes [this]
    [root])

  p/IHaveSlots
  (dom-slot [this id]
    (let [slot (get slots id)]
      (when-not slot
        (throw (ex-info "component did not define slot" {:id id :this this})))

      (when-not (.-parentNode slot)
        (throw (ex-info "slot was not added in render" {:id id})))

      slot))

  p/IUpdatable
  (supports? [this next]
    (and (component-node? next)
         (let [other (.-component ^ComponentNode next)]
           (if (fn? other)
             (identical? render-fn other)
             (identical? config other)))))

  (dom-sync! [this ^ComponentNode next]
    ;; FIXME: let (:should-update config) decide if update should happen or not?
    (when (or dirty? (not= props (.-props next)))
      (let [prev-props props
            next-props (.-props next)]
        (set! props next-props)
        (.component-render! this)
        ;; FIXME: did-update before/after props
        ))
    :synced)

  p/IAmStateful
  (invalidate! [this]
    (set! dirty? true)
    (p/schedule-update! (::scheduler parent-env) this))

  p/ITreeNode
  (sync! [this]
    (when dirty?
      (.component-render! this))

    (p/sync! root))

  p/IHandleEvents
  (handle-event! [this ev-id e ev-args]
    ;; fn-sig should be (fn [env e ...])
    ;; but some generic event handlers may want to know the ev-id
    ;; but putting that in there seems messy
    ;; (fn [env ev-id e ...]) is too noisy
    ;; (fn [env e ev-id ...]) sucks with args
    ;; most event handlers probably won't care so just adding it to e
    (gobj/set e "__shadow$ev" ev-id)

    (when-let [handler (get config ev-id)]
      (run!
        #(apply %1 component-env e ev-args)
        (if (fn? handler)
          [handler]
          handler)))

    (when-let [parent (::component parent-env)]
      (p/handle-event! parent ev-id e ev-args)))

  p/IDestructible
  (destroyed? [this]
    destroyed?)

  (destroy! [this]
    (set! destroyed? true)

    (when-let [handler (:will-destroy config)]
      (handler component-env state))

    (p/destroy! root))

  Object
  ;; can't do this in the ComponentNode.as-managed since we need the this pointer
  (component-init! [^ComponentInstance this]
    (let [{:keys [init-state]} config

          child-env
          (-> parent-env
              (update ::depth safe-inc)
              (assoc ::parent (::component parent-env)
                     ::dom-refs (atom {})
                     ::component-id (next-component-id)
                     ::component this
                     ::config config))

          init-state
          (cond
            (fn? init-state)
            (init-state props)

            (some? init-state)
            init-state

            :else
            {})]

      (set! component-env child-env)
      (set! root (common/ManagedRoot. child-env nil nil))
      (set! state init-state)
      (.component-render! this)))

  (component-render! [^ComponentInstance this]
    (let [frag (render-fn component-env props state)]
      (set! dirty? false)
      (p/update! root  frag))))

(set! *warn-on-infer* true)

(defn component-create [env component props]
  ;; FIXME: have component declare them properly
  (let [slots {:default (common/marker env)}]

    (cond
      (map? component)
      (doto (ManagedComponent. env component props nil nil (:render component) nil false false slots)
        (.component-init!))

      (fn? component)
      (doto (ManagedComponent. env {} props nil nil component nil false false slots)
        (.component-init!)))))

(deftype ComponentNode [component props]
  p/IConstruct
  (as-managed [this env]
    (component-create env component props))

  IEquiv
  (-equiv [this ^ComponentNode other]
    (and (instance? ComponentNode other)
         (identical? component (.-component other))
         (= props (.-props other)))))

(defn component-node? [x]
  (instance? ComponentNode x))

(deftype SlotNode [node]
  p/IConstruct
  (as-managed [this env]
    this)

  p/IManageNodes
  (dom-first [this] node)
  (dom-insert [this parent anchor]
    (when (.-parentNode node)
      (throw (ex-info "slot already in document" {})))

    (.insertBefore parent node anchor))

  p/ITraverseNodes
  (managed-nodes [this] [])

  p/IUpdatable
  (supports? [this ^SlotNode other]
    (and (instance? SlotNode other)
         (identical? node (.-node other))))

  (dom-sync! [this other])

  p/IDestructible
  (destroy! [this]
    (.remove node)))

(defn slot [{::keys [component] :as env}]
  (let [slots (.-slots component)]
    (SlotNode. (get slots :default))))

(declare ChildrenNode)

(deftype ManagedChildren
  [env
   ^:mutable component
   ^:mutable children
   ^:mutable nodes]

  p/IManageNodes
  (dom-first [this]
    (p/dom-first component))

  (dom-insert [this parent anchor]
    (p/dom-insert component parent anchor)
    (doseq [node nodes]
      (let [slot (p/dom-slot component :default)]
        (p/dom-insert node (.-parentNode slot) slot))))

  p/ITraverseNodes
  (managed-nodes [this]
    nodes)

  p/IUpdatable
  (supports? [this ^ChildrenNode next]
    (and (instance? ChildrenNode next)
         (p/supports? component (.-component-node next))))

  (dom-sync! [this ^ChildrenNode next]
    (p/dom-sync! component (.-component-node next))

    (let [old-children children
          new-children (.-children next)

          oc (count old-children)
          nc (count new-children)
          m (js/Math.min oc nc)]

      (dotimes [i m]
        (let [old (nth old-children i)
              new (nth new-children i)]
          (when (not= old new)

            (let [node (nth nodes i)]
              (if (p/supports? node new)
                (p/dom-sync! node new)
                (let [new-node (common/replace-managed env node new)]
                  (set! nodes (assoc nodes i new-node))))))))

      (cond
        ;; less children
        (> oc nc)
        (throw (ex-info "TBD, changed number of children" {}))

        ;; more children
        (> nc oc)
        (throw (ex-info "TBD, changed number of children" {}))
        )))

  p/IDestructible
  (destroy! [this]
    (run! #(p/destroy! %) nodes)
    (p/destroy! component)))


(deftype ChildrenNode [component-node children]
  p/IConstruct
  (as-managed [this env]
    (let [comp
          (p/as-managed component-node env)

          nodes
          (into [] (map #(p/as-managed % env) children))]

      (ManagedChildren. env comp children nodes)))

  IEquiv
  (-equiv [this ^ChildrenNode other]
    (and (instance? ComponentNode other)
         (= component-node (.-component-node other))
         (= children (.-children other)))))

(defn call-event-fn [{::keys [component scheduler] :as env} ev-id e ev-args]
  (when-not component
    (throw (ex-info "event handlers can only be used in components" {:env env :ev-id ev-id :e e :ev-args ev-args})))

  (p/schedule-event! scheduler #(p/handle-event! component ev-id e ev-args)))

(defn event-attr [env node event oval [ev-id & ev-args :as nval]]

  (when ^boolean js/goog.DEBUG
    (when-not (vector? nval)
      (throw (ex-info "event handler expects a vector arg" {:event event :node node :nval nval})))

    (when-not (keyword? ev-id)
      (throw (ex-info "event handler must start with [::keyword ...]" {:event event :node node :nval nval}))))


  (let [ev-key (str "__shadow$" (name event))]
    (when-let [ev-fn (gobj/get node ev-key)]
      (.removeEventListener node ev-fn))

    ;(js/console.log "adding ev fn" val)

    (let [ev-fn #(call-event-fn env ev-id % ev-args)
          ev-opts #js {}]

      ;; FIXME: need to track if once already happened. otherwise may re-attach and actually fire more than once
      ;; but it should be unlikely to have a changing val with ^:once?
      (when-let [m (meta nval)]
        (when (:once m)
          (gobj/set ev-opts "once" true))

        (when (:passive m)
          (gobj/set ev-opts "passive" true)))

      ;; FIXME: ev-opts are not supported by all browsers
      ;; closure lib probably has something to handle that
      (.addEventListener node (name event) ev-fn ev-opts)

      (gobj/set node ev-key ev-fn))))

(defmethod p/set-attr* :on-click [env node _ oval nval]
  (event-attr env node :click oval nval))

(defmethod p/set-attr* :on-dblclick [env node _ oval nval]
  (event-attr env node :dblclick oval nval))

(defmethod p/set-attr* :on-keydown [env node _ oval nval]
  (event-attr env node :keydown oval nval))

(defmethod p/set-attr* :on-change [env node _ oval nval]
  (event-attr env node :change oval nval))

(defmethod p/set-attr* :on-blur [env node _ oval nval]
  (event-attr env node :blur oval nval))

(defmethod p/set-attr* :shadow.arborist/ref [{::keys [dom-refs] :as env} node _ oval nval]
  (when-not dom-refs
    (throw (ex-info "ref used outside component" {:val nval :env env})))
  (when (and oval (not= oval nval))
    (swap! dom-refs dissoc oval))
  (swap! dom-refs assoc nval node))

