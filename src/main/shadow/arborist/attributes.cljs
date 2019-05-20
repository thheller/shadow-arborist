(ns shadow.arborist.attributes
  (:require
    [goog.object :as gobj]
    [clojure.string :as str]
    [shadow.arborist.protocols :as p]
    ))

;; FIXME: keep this code short, due to the set-attr* multimethod nothing this uses will ever be removed

(defn vec->class [v]
  (reduce
    (fn [s c]
      (cond
        (not c)
        s

        (not s)
        c

        :else
        (str s " " c)))
    nil
    v))

(defn map->class [m]
  (reduce-kv
    (fn [s ^not-native k v]
      (cond
        (not v)
        s

        (not s)
        (-name k)

        :else
        (str s " " (-name k))))
    nil
    m))

;; FIXME: behave like goog.dom.setProperties?
;; https://github.com/google/closure-library/blob/31e914b9ecc5c6918e2e6462cbbd4c77f90be753/closure/goog/dom/dom.js#L453
(defmethod p/set-attr* ::p/default [env ^js node ^not-native key oval nval]
  (if nval
    (.setAttribute node (-name key) nval)
    (.removeAttribute node (-name key))))

(defmethod p/set-attr* :for [env ^js node _ oval nval]
  (set! node -htmlFor nval))

(defmethod p/set-attr* :style [env ^js node _ oval nval]
  (cond
    (map? nval)
    (let [style (.-style node)]
      (reduce-kv
        (fn [_ ^not-native k v]
          (gobj/set style (-name k) v))
        nil
        nval))

    (string? nval)
    (set! (.. node -style -cssText) nval)

    :else
    (throw (ex-info "invalid value for :class" {:node node :val nval}))
    ))

(defmethod p/set-attr* :class [env ^js node _ oval nval]
  (cond
    (string? nval)
    (set! node -className nval)

    ;; FIXME: classlist?
    (vector? nval)
    (if-let [s (vec->class nval)]
      (set! node -className s)
      (set! node -className ""))

    (map? nval)
    (if-let [s (map->class nval)]
      (set! node -className s)
      ;; FIXME: removeAttribute? nil?
      (set! node -className ""))

    :else
    (throw (ex-info "invalid value for :class" {:node node :val nval}))
    ))



