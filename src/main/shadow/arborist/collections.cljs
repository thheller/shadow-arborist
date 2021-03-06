(ns shadow.arborist.collections
  (:require
    [shadow.arborist.protocols :as p]
    [shadow.arborist.fragments :as frag]))

(declare CollectionNode)

(defn index-map ^not-native [key-vec]
  (persistent! (reduce-kv #(assoc! %1 %3 %2) (transient {}) key-vec)))

(deftype ManagedCollection
  [env
   ^:mutable coll
   ^:mutable key-fn
   ^:mutable render-fn
   ^:mutable ^not-native items ;; map of {key managed}
   ^:mutable item-keys ;; vector of (key-fn item)
   ^:mutable item-vals ;; map of {key rendered}
   marker-before
   marker-after
   ]

  p/IManageNodes
  (dom-first [this] marker-before)

  (dom-insert [this parent anchor]
    (.insertBefore parent marker-before anchor)
    (run! #(p/dom-insert (get items %) parent anchor) item-keys)
    (.insertBefore parent marker-after anchor))

  p/ITraverseNodes
  (managed-nodes [this]
    (mapv #(get items %) item-keys))

  p/ITreeNode
  (sync! [this]
    (run!
      (fn [key]
        (let [item (get items key)]
          (p/sync! item)))
      item-keys))

  p/IUpdatable
  (supports? [this next]
    (instance? CollectionNode next))

  (dom-sync! [this ^CollectionNode next]
    (let [old-coll coll
          new-coll (vec (.-coll next)) ;; FIXME: could use into-array
          dom-parent (.-parentNode marker-after)]

      (when-not dom-parent
        (throw (ex-info "sync while not in dom?" {})))

      (set! coll new-coll)
      (set! key-fn (.-key-fn next))
      (set! render-fn (.-render-fn next))

      (let [old-keys item-keys
            old-indexes (index-map old-keys)
            new-keys (into [] (map key-fn) coll)

            updated
            (loop [anchor marker-after
                   idx (-> new-keys count dec)
                   updated (transient #{})]
              (if (neg? idx)
                (persistent! updated)
                (let [key (nth new-keys idx)
                      item (get items key)
                      data (nth new-coll idx)
                      updated (conj! updated key)]

                  (if-not item
                    ;; new item added to list, render normally and insert
                    (let [rendered (render-fn data idx key)
                          item (p/as-managed rendered env)]

                      (p/dom-insert item (.-parentNode anchor) anchor)
                      (set! item-vals (assoc item-vals key rendered))
                      (set! items (assoc items key item))
                      (recur (p/dom-first item) (dec idx) updated))

                    ;; item did exist, re-render and maybe move
                    (let [rendered (render-fn data idx key)]

                      ;; skip dom-sync if render result is equal
                      (if (= rendered (get item-vals key))
                        (let [next-anchor (p/dom-first item)]

                          ;; still may need to move though
                          (when (not= idx (get old-indexes key))
                            (p/dom-insert item dom-parent anchor))

                          (recur next-anchor (dec idx) updated))

                        (do (set! item-vals (assoc item-vals key rendered))
                            (if (p/supports? item rendered)
                              ;; update in place if supported
                              (do (p/dom-sync! item rendered)
                                  (let [next-anchor (p/dom-first item)]

                                    ;; FIXME: this is probably not ideal
                                    (when (not= idx (get old-indexes key))
                                      (p/dom-insert item dom-parent anchor))

                                    (recur next-anchor (dec idx) updated)))

                              ;; not updateable, swap
                              (let [new-item (p/as-managed rendered env)]
                                (set! items (assoc items key new-item))
                                (p/dom-insert new-item dom-parent anchor)
                                (p/destroy! item)

                                (recur (p/dom-first new-item) (dec idx) updated)
                                )))))))))]

        (set! item-keys new-keys)

        ;; remove old items/render results
        (reduce-kv
          (fn [_ key item]
            (when-not (contains? updated key)
              (p/destroy! item)
              (set! items (dissoc items key))
              (set! item-vals (dissoc item-vals key))))
          nil
          items)))
    :synced)

  p/IDestructible
  (destroy! [this]
    (.remove marker-before)
    (when items
      (reduce-kv
        (fn [_ _ item]
          (p/destroy! item))
        nil
        items))
    (.remove marker-after)))

(deftype CollectionNode [coll key-fn render-fn]
  p/IConstruct
  (as-managed [this env]
    (let [coll (vec coll) ;; FIXME: could use into-array, colls are never modified again, only used to look stuff up by index
          len (count coll)
          marker-before (js/document.createComment "coll-start")
          marker-after (js/document.createComment "coll-end")]

      (loop [idx 0
             items (transient {})
             keys (transient [])
             vals (transient {})]

        (if (>= idx len)
          (ManagedCollection.
            env
            coll
            key-fn
            render-fn
            (persistent! items)
            (persistent! keys)
            (persistent! vals)
            marker-before
            marker-after)

          (let [val (nth coll idx)
                key (key-fn val)
                rendered (render-fn val idx key)
                managed (p/as-managed rendered env)]

            (recur
              (inc idx)
              (assoc! items key managed)
              (conj! keys key)
              (assoc! vals key rendered)))))))

  IEquiv
  (-equiv [this ^CollectionNode other]
    (and (instance? CollectionNode other)
         ;; could be a keyword, can't use identical?
         (keyword-identical? key-fn (.-key-fn other))
         ;; FIXME: this makes it never equal if fn is created in :render fn
         (identical? render-fn (.-render-fn other))
         ;; compare coll last since its pointless if the others changed and typically more expensive to compare
         (= coll (.-coll other)))))

(defn node [coll key-fn render-fn]
  {:pre [(sequential? coll)
         (ifn? key-fn)
         (fn? render-fn)]}
  (CollectionNode. coll key-fn render-fn))