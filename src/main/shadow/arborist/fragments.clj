(ns shadow.arborist.fragments
  (:require [clojure.string :as str]))

(defn parse-tag [spec]
  (let [spec (name spec)
        fdot (.indexOf spec ".")
        fhash (.indexOf spec "#")]
    (cond
      (and (= -1 fdot) (= -1 fhash))
      [spec nil nil]

      (= -1 fhash)
      [(subs spec 0 fdot)
       nil
       (str/replace (subs spec (inc fdot)) #"\." " ")]

      (= -1 fdot)
      [(subs spec 0 fhash)
       (subs spec (inc fhash))
       nil]

      (> fhash fdot)
      (throw (str "cant have id after class?" spec))

      :else
      [(subs spec 0 fhash)
       (subs spec (inc fhash) fdot)
       (str/replace (.substring spec (inc fdot)) #"\." " ")])))

(defn const? [thing]
  (or (string? thing)
      (number? thing)
      (boolean? thing)
      (keyword? thing)
      (= thing 'nil)
      (and (vector? thing) (every? const? thing))
      (and (map? thing)
           (reduce-kv
             (fn [r k v]
               (if-not (and (const? k) (const? v))
                 (reduced false)
                 r))
             true
             thing))))

(defn next-el-id [{:keys [el-seq-ref] :as env}]
  (swap! el-seq-ref inc))

(defn make-code [{:keys [code-ref] :as env} code {:keys [element-id] :as extra}]
  ;; FIXME: de-duping code may eliminate wanted side effects?
  ;; (<< [:div (side-effect) (side-effect)])
  ;; this is probably fine since the side-effect should be done elsewhere anyways
  (let [id (or (get @code-ref code)
               (let [next-id (count @code-ref)]
                 (swap! code-ref assoc code next-id)
                 next-id))]

    (merge
      extra
      {:op :code-ref
       :parent (:parent env)
       :sym (if element-id
              (symbol (str "d" element-id)) ;; dynamic "elements"
              (gensym))
       :ref-id id})))

(declare analyze-node)

(defn analyze-component [env [tag component attrs & children :as el]]
  (assert (> (count el) 2))

  (let [id (next-el-id env)
        el-sym (symbol (str "c" id))

        child-env
        (assoc env :parent [:component el-sym])]

    {:op :component
     :parent (:parent env)
     :component (make-code env component {})
     :attrs (make-code env attrs {})
     :element-id id
     :sym el-sym
     :src el
     :children (into [] (map #(analyze-node child-env %)) children)}))

(defn with-loc [{:keys [src] :as ast} form]
  (if-not src
    form
    (let [m (meta src)]
      (if-not m
        form
        (with-meta form m)))))

(defn analyze-dom-element [env [tag-kw attrs :as el]]
  (let [[attrs children]
        (if (and attrs (map? attrs))
          [attrs (subvec el 2)]
          [nil (subvec el 1)])

        [tag html-id html-class]
        (parse-tag tag-kw)

        id (next-el-id env)

        el-sym (symbol (str "el" id "_" tag))

        _ (when (and html-id (:id attrs))
            (throw (ex-info "cannot have :id attribute AND el#id" {:tag-kw tag-kw :attrs attrs})))

        _ (when (and html-class (:class attrs))
            (throw (ex-info "cannot have :class attribute AND el.class" {:tag-kw tag-kw :attrs attrs})))

        attrs
        (-> attrs
            (cond->
              html-id
              (assoc :id html-id)

              html-class
              (assoc :class html-class)))

        tag (keyword (namespace tag-kw) tag)

        attr-ops
        (->> attrs
             (map (fn [[attr-key attr-value]]
                    (if (const? attr-value)
                      {:op :static-attr
                       :el el-sym
                       :element-id id
                       :attr attr-key
                       :value attr-value
                       :src attrs}

                      ;; FIXME: this could be smarter and pre-generate "partially" dynamic attrs
                      ;; :class ["hello" "world" (when x "foo")]
                      ;; could build ["hello "world" nil] once
                      ;; and then (assoc the-const 2 (when x "foo")) before passing it along
                      ;; :style {:color "red" :font-size x}
                      ;; (assoc the-const :font-size x)
                      ;; probably complete overkill but could be fun
                      {:op :dynamic-attr
                       :el el-sym
                       :element-id id
                       :attr attr-key
                       :value (make-code env attr-value {})
                       :src attrs}
                      )))
             (into []))

        child-env
        (assoc env :parent [:element el-sym])]

    {:op :element
     :parent (:parent env)
     :element-id id
     :sym el-sym
     :tag tag
     :src el
     :children
     (-> []
         (into attr-ops)
         (into (map #(analyze-node child-env %)) children))}))

(defn analyze-element [env el]
  (assert (pos? (count el)))

  (let [tag-kw (nth el 0)]
    (when-not (keyword? tag-kw)
      (throw (ex-info "vector must start with a keyword" {:el el})))

    (if (= :> tag-kw)
      (analyze-component env el)
      (analyze-dom-element env el))))

(defn analyze-text [env node]
  {:op :text
   :parent (:parent env)
   :sym (gensym)
   :text node})

(defn analyze-node [env node]
  (cond
    (vector? node)
    (analyze-element env node)

    (string? node)
    (analyze-text env node)

    (number? node)
    (analyze-text env (str node))

    :else
    (make-code env node {:element-id (next-el-id env)})))

(defn reduce-> [init reduce-fn coll]
  (reduce reduce-fn init coll))

(defn make-build-impl [ast]
  (let [this-sym (gensym "this")
        env-sym (with-meta (gensym "env") {:tag 'not-native})
        vals-sym (with-meta (gensym "vals") {:tag 'not-native})

        {:keys [bindings mutations return nodes] :as result}
        (reduce
          (fn step-fn [env {:keys [op sym parent] :as ast}]
            (let [[parent-type parent-sym] parent]
              (case op
                :element
                (-> env
                    (update :bindings conj sym (with-loc ast `(create-element ~env-sym ~(:tag ast))))
                    (update :return conj sym)
                    (cond->
                      (and parent-sym (= parent-type :element))
                      (update :mutations conj (with-loc ast `(append-child ~parent-sym ~sym)))

                      (and parent-sym (= parent-type :component))
                      (update :mutations conj (with-loc ast `(component-append ~parent-sym ~sym)))

                      (not parent-sym)
                      (update :nodes conj sym))
                    (reduce-> step-fn (:children ast))
                    )

                :component
                (-> env
                    (update :bindings conj sym
                      (with-loc ast
                        `(component-create ~env-sym
                           (aget ~vals-sym ~(-> ast :component :ref-id))
                           (aget ~vals-sym ~(-> ast :attrs :ref-id)))))
                    (update :return conj sym)
                    (cond->
                      parent-sym
                      (update :mutations conj (with-loc ast `(append-managed ~parent-sym ~sym)))
                      (not parent-sym)
                      (update :nodes conj sym))
                    (reduce-> step-fn (:children ast)))

                ;; text nodes can never mutate. no need to return them
                ;;
                :text
                (-> env
                    (update :bindings conj sym `(create-text ~env-sym ~(:text ast)))
                    (cond->
                      parent-sym
                      (update :mutations conj `(append-child ~parent-sym ~sym))

                      (not parent-sym)
                      (update :nodes conj sym)
                      ))

                :code-ref
                (-> env
                    (update :bindings conj sym (with-loc ast `(create-managed ~env-sym (aget ~vals-sym ~(:ref-id ast)))))
                    (cond->
                      parent-sym
                      (update :mutations conj (with-loc ast `(append-managed ~parent-sym ~sym)))

                      (not parent-sym)
                      (update :nodes conj sym))
                    (update :return conj sym))

                :static-attr
                (-> env
                    (update :mutations conj (with-loc ast `(set-attr ~env-sym ~(:el ast) ~(:attr ast) nil ~(:value ast)))))

                :dynamic-attr
                (-> env
                    (update :mutations conj (with-loc ast `(set-attr ~env-sym ~(:el ast) ~(:attr ast) nil (aget ~vals-sym ~(-> ast :value :ref-id))))
                      ))))
            )
          {:bindings []
           :mutations []
           :return []
           :nodes []}
          ast)]


    `(fn [~env-sym ~vals-sym]
       (let [~@bindings]
         ~@mutations
         (cljs.core/array (cljs.core/array ~@nodes) (cljs.core/array ~@return))))))

(defn make-update-impl [ast]
  (let [this-sym (gensym "this")
        env-sym (gensym "env")
        nodes-sym (gensym "nodes")
        roots-sym (gensym "roots")
        oldv-sym (gensym "oldv")
        newv-sym (gensym "newv")

        {:keys [bindings mutations return nodes] :as result}
        (reduce
          (fn step-fn [env {:keys [op sym element-id] :as ast}]
            (case op
              :element
              (-> env
                  (assoc-in [:sym->id sym] element-id)
                  (reduce-> step-fn (:children ast)))

              :component
              (-> env
                  (update :mutations conj
                    (with-loc ast
                      `(component-update
                         ~env-sym
                         ~roots-sym
                         ~nodes-sym
                         ~(:element-id ast)
                         (aget ~oldv-sym ~(-> ast :component :ref-id))
                         (aget ~newv-sym ~(-> ast :component :ref-id))
                         (aget ~oldv-sym ~(-> ast :attrs :ref-id))
                         (aget ~newv-sym ~(-> ast :attrs :ref-id)))))
                  (reduce-> step-fn (:children ast)))

              :text
              env

              :code-ref
              (-> env
                  (update :mutations conj
                    (let [ref-id (:ref-id ast)]
                      (with-loc ast
                        `(update-managed
                           ~env-sym
                           ~roots-sym
                           ~nodes-sym
                           ~(:element-id ast)
                           (aget ~oldv-sym ~ref-id)
                           (aget ~newv-sym ~ref-id))))))

              :static-attr
              env

              :dynamic-attr
              (let [ref-id (-> ast :value :ref-id)
                    form
                    `(update-attr ~env-sym ~nodes-sym ~element-id ~(:attr ast) (aget ~oldv-sym ~ref-id) (aget ~newv-sym ~ref-id))]

                (-> env
                    (update :mutations conj form))))
            )
          {:mutations []
           :sym->id {}}
          ast)]


    `(fn [~env-sym ~roots-sym ~nodes-sym ~oldv-sym ~newv-sym]
       ~@mutations)))

(defn make-fragment [macro-env body]
  (let [env
        {:code-ref (atom {})
         :el-seq-ref (atom -1) ;; want 0 to be first id
         ;; :parent (gensym "root")
         }

        ast
        (mapv #(analyze-node env %) body)

        code-snippets
        (->> @(:code-ref env)
             (sort-by val)
             (map (fn [[snippet id]]
                    snippet))
             (into []))

        code-sym
        (gensym)

        ;; FIXME: use something that is guaranteed unique but shorter
        ;; can't use gensym because of caching (might cause conflicts when re-used)
        ;; it is useful to have the code location for debugging maybe but long string aren't that useful
        ;; in production build and just make everything bigger (not much but still)
        frag-id (str (-> macro-env :ns :name) "@" (:line macro-env) ":" (:column macro-env))]

    `(let [~code-sym (cljs.core/array ~@code-snippets)
           frag-id# ~frag-id]

       (or (fragment-get frag-id# ~code-sym)
           (fragment-reg frag-id# ~code-sym ~(make-build-impl ast) ~(make-update-impl ast)))
       )))

