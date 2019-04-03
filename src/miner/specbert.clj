(ns miner.specbert
  (:require [clojure.spec-alpha2 :as s]
            [clojure.spec-alpha2.gen :as gen]
            [clojure.spec-alpha2.test :as test]))


;; Wait a second, we need a better microbenchmark!!!

(def mxx #:user {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8 :i 9 :j 10 :k 11 :l 12 :m 13})

(defn ben [f]
  (dotimes [n 10] (println (time (f (assoc mxx :user/xx n))))))

;; should be namespaceable?, but probably faster to inline in kspace
;; also, logically same as #(instance? clojure.lang.Named %)
#_ (defn namespaced? [x]
  (or (keyword? x) (symbol? x)))

(defn kspace [x]
  (when (instance? clojure.lang.Named x)
    (namespace x)))


;; looks pretty nice with when-first
;; also fastest by criterium: 128ns (mapspace mxx)
(defn mapspace [m]
  (when-first [[k1] m]
    (when-let [ksp (kspace k1)]
      (reduce-kv (fn [res k _]
                   (if (= res (kspace k))
                     res
                     (reduced nil)))
                 ksp
                 m))))

;; short, but much slower
(defn mapspace1 [m]
  (let [ks (keys m)
        msp (kspace (first ks))]
    (when (and msp (every? #{msp} (map kspace (rest ks))))
      msp)))




;; My first try.  I don't like the ugly cond, but it's pretty fast
(defn mapspace3 [m]
  (reduce-kv (fn [res k _]
               (let [nspace (kspace k)]
                 (cond
                     (nil? nspace) (reduced nil)
                     (nil? res) nspace
                     (= res nspace) nspace
                   :else (reduced nil))))
             nil
             m))



(defn mapspace2 [m]
  (let [ks (keys m)
        msp (kspace (first ks))]
    (when msp
      (loop [ks (rest ks) msp msp]
        (if (seq ks)
          (when (= msp (kspace (first ks)))
            (recur (rest ks) msp))
          msp)))))


;; ok
(defn mapspace4 [m]
  (when-let [es (seq m)]
    (when-let [msp (kspace (key (first es)))]
      (loop [es (rest es) msp msp]
        (if (seq es)
          (when (= msp (kspace (key (first es))))
            (recur (rest es) msp))
          msp)))))


;; but slowest
(defn mapspace5 [m]
  (let [ss (sequence (comp (map key) (map kspace)) m)]
    (when-first [msp ss]
      (when (every? #{msp} (rest ss))
        msp))))
      

(defn samesp [base ksp]
  (cond
      (nil? ksp) (reduced nil)
      (nil? base) ksp
      (= base ksp)  base
    :else (reduced nil)))

;; slow
(defn tmapspace [m]
  (transduce (comp (map key) (map kspace)) (completing samesp) nil m))



(defn same-vals? [m]
  (when (seq m)
    (apply = (vals m))))

(defn vals= [m]
  (when-first [[_ v] m]
    (every? #(= v %) (vals m))))

;; fastest by far
(defn rvals= [m]
  (when-first [[_ v1] m]
    (reduce-kv (fn [res _ v] (if (= res v) res (reduced nil))) v1 m)))
  


(defn strip-ns
  [named]
  (if (symbol? named)
    (symbol nil (name named))
    (keyword nil (name named))))

;; core_print.clj 1.9-alpha8
(defn lift-ns
  "Returns [lifted-ns lifted-map] or nil if m can't be lifted."
  [m]
  (loop [ns nil
         [[k v :as entry] & entries] (seq m)
         lm (empty m)]
    (if entry
      (when (or (keyword? k) (symbol? k))
        (if ns
          (when (= ns (namespace k))
            (recur ns entries (assoc lm (strip-ns k) v)))
          (when-let [new-ns (namespace k)]
            (recur new-ns entries (assoc lm (strip-ns k) v)))))
      [ns lm])))

;; might be better with reduce-kv
;; but multi-state might be needed

;; inlining kspace and strip-ns is slightly faster.  Worth it for benchmarking, not sure
;; about clarity and maintenance.

(defn lft [m]
  (when-first [[k1] m]
    (when-let [ksp (when (or (keyword? k1) (symbol? k1)) (namespace k1))]
      (when-let [bare (reduce-kv (fn [bare k v]
                                   (if (and (or (keyword? k) (symbol? k)) (= ksp (namespace k)))
                                     (assoc bare (if (keyword? k)
                                                   (keyword nil (name k))
                                                   (symbol nil (name k)))
                                            v)
                                     (reduced nil)))
                                 (empty m)
                                 m)]
        [ksp bare]))))


;; pretty, but slower
(defn baremap [m]
  (reduce-kv (fn [res k v]
               (assoc res (strip-ns k) v))
             (empty m)
             m))

;; note: transients don't help above

;; prettier but not faster than lift-ns
(defn my-lift-ns [m]
  (when-let [ns (mapspace m)]
    [ns (baremap m)]))



;; BUT, really suspicious of whole "make a new map with bare keys" approach.  My guess is
;; that it would be better to print the keys specially rather than rebuild a map.

(comment
#:foo {:a 1 :b 2}

#{1 2 3}
)


(comment
  (require '[criterium.core :as c])
  (use 'miner.specbert)

  (assert (= (lift-ns mxx) (lft mxx)))
  
  (do (c/quick-bench (lift-ns mxx)) (c/quick-bench (lft mxx)) (c/quick-bench (my-lift-ns mxx)))

  (let [nom (assoc mxx "x" 42)] (c/quick-bench (lift-ns nom)) (c/quick-bench (lft nom)) (c/quick-bench (my-lift-ns nom)))
  
  )


;; -------------

;; http://dev.clojure.org/jira/browse/CLJ-1959
;; proposed patch

(defn map-vals
  "Returns a lazy hashmap consisting of the result of applying f to
  the value of each set in hashmap.
  Function f should accept one single argument."
  {:added "1.9"}
  [f m]
  (persistent!
    (reduce-kv (fn [m k v] (assoc! m k (f v)))
               (transient (empty m)) m)))

(defn map-keys
  [f m]
  "Returns a lazy hashmap consisting of the result of applying f to
  the key of each set in hashmap.
  Function f should accept one single argument."
  {:added "1.9"}
  (persistent!
    (reduce-kv (fn [m k v] (assoc! m (f k) v))
               (transient (empty m)) m)))

;; see also my group-by-kv in hack/grouping.clj


;; My private comments: The result is the same type as the original m (not necessarily a
;; hashmap), and the functions are not lazy -- reduce-kv is eager.


(defn zmapk [f m]
  (zipmap (map f (keys m)) (vals m)))

(defn zmapv [f m]
  (zipmap (keys m) (map f (vals m))))

(defn lmapk [f m]
  (into (empty m) (map (juxt #(f (key %)) val)) m))

(defn lmapv [f m]
  (into (empty m) (map (juxt key #(f (val %)))) m))

(defn tmapk [f m]
  (transduce (map #(clojure.lang.MapEntry. (key %) (f (val %))))
             conj
             (empty m)
             m))

(defn tmapv [f m]
  (transduce (map #(clojure.lang.MapEntry. (f (key %)) (val %)))
             conj
             (empty m)
             m))


(defn emapv [f m]
  (into (empty m) (map #(clojure.lang.MapEntry. (key %) (f (val %)))) m))

(defn emapk [f m]
  (into (empty m) (map #(clojure.lang.MapEntry. (f (key %)) (val %))) m))



;; CLJ-1945 spec to disallow extra keys

#_ (defn only-keys? [ks m]
  (let [ks (set ks)]
    (reduce-kv (fn [res k v] (if (contains? ks k) res (reduced false))) true m)))

(defn extra-keys? [ks m]
  (let [ks (set ks)]
    (or (> (count m) (count ks))
        (reduce-kv (fn [res k v] (if (contains? ks k) res (reduced true))) false m))))

(defn only-keys? [ks]
  (fn [m] (not (extra-keys? ks m))))





(defn bar [kw i]
  {:pre [(int? i) (keyword? kw)]
   :post [(symbol? %)]}
  (symbol (namespace kw) (str (name kw) i)))


(s/fdef bar
    :args (s/cat :kw keyword? :n pos-int?)
    :ret symbol?)

