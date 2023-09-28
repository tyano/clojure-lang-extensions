(ns org.clojars.t-yano.lang.core
  (:refer-clojure :exclude [group-by])
  (:require [clojure.core :as core]
            [clojure.string :as string]))

(defn group-by
  ([key-fn val-fn coll]
   (persistent!
    (reduce
     (fn [m v]
       (let [group-key (key-fn v)
             value     (val-fn v)]
         (assoc! m group-key (conj (get m group-key []) value))))
     (transient {})
     coll)))

  ([key-fn coll]
   (core/group-by key-fn coll)))

(defn select-by-ns
  [m ns-value]
  (let [ns-name (name ns-value)]
    (persistent!
     (reduce
      (fn [m [k v]]
        (if (= ns-name (namespace k))
          (assoc! m k v)
          m))
      (transient {})
      m))))

(defn map-keys
  [m f]
  (persistent!
   (reduce
    (fn [m [k v]] (assoc! m (f k) v))
    (transient {})
    m)))

(defn convert-keys
  [m f]
  (persistent!
   (reduce
    (fn [m [k v]]
      (if-let [new-key (f k)]
        (assoc! m new-key v)
        (assoc! m k v)))
    (transient {})
    m)))

(defn map-keys-recursive
  [m f]
  (letfn [(process-value
            [v]
            (cond
              (map? v)
              (map-keys-recursive v f)

              (sequential? v)
              (map (fn [sv] (process-value sv)) v)

              :else
              v))]
    (persistent!
     (reduce
      (fn [m [k v]] (assoc! m (f k) (process-value v)))
      (transient {})
      m))))

(defn keep-keys
  [m f]
  (persistent!
   (reduce
    (fn [m [k v]]
      (if-let [new-key (f k)]
        (assoc! m new-key v)
        m))
    (transient {})
    m)))

(defn remove-ns-part
  [v]
  {:pre [(keyword? v)]}
  (let [n (name v)]
    (keyword n)))

(defn blank?
  [str]
  (if str
    (string/blank? (string/replace str "　" ""))
    true))

(defn collection?
  [data]
  (and (coll? data) (not (map? data))))

(defn flatten-map
  [m]
  (-> m seq flatten))

(defn distinct-by
  "Returns a lazy sequence of the elements of coll with duplicates removed.
  Returns a stateful transducer when no collection is provided."
  ([key-fn]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (let [check-value (key-fn input)]
            (if (contains? @seen check-value)
              result
              (do (vswap! seen conj check-value)
                  (rf result input)))))))))
  ([key-fn coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                 ((fn [[f :as xs] seen]
                    (when-let [s (seq xs)]
                      (let [check-value (key-fn f)]
                        (if (contains? seen check-value)
                          (recur (rest s) seen)
                          (cons f (step (rest s) (conj seen check-value)))))))
                  xs seen)))]
     (step coll #{}))))

(defn assoc-by-key
  [key-fn coll]
  (persistent!
   (reduce
    (fn [m v]
      (let [k (key-fn v)]
        (assoc! m k v)))
    (transient {})
    coll)))