(ns io.dominic.ednup.core
  (:require
    [rewrite-clj
     [node :as n]
     [zip :as z]
     [parser :as parser]]
    [potemkin :refer  [def-map-type]]))

(comment
  (-> (z/of-file "deps.edn")
      (z/get :deps)
      (z/assoc 'foo.bar/bar {:mvn/version "20.1"})
      (z/get 'foo.bar/bar)
      (z/left)
      (z/left)
      (z/append-newline)
      z/root-string))

;; TODO: This function leaves a trail of nil's behind it. I need some kind of
;; method for tracking dummy values and cleaning them up / never inserting them
;; really.
(defn- z-get
  [zloc k]
  (let [zloc (if (nil? (z/sexpr zloc))
               (z/replace zloc {})
               zloc)]
    (or (z/get zloc k)
        (z/get (z/assoc zloc k nil) k))))

(defn- z-assoc
  [zloc k v]
  (z/assoc (if (nil? (z/sexpr zloc))
             (z/replace zloc {})
             zloc)
           k
           v))

(def-map-type RewriteMap [zloc]
  (get [this k default-value]
       (if-let [v (z-get zloc k)]
         (RewriteMap. v)
         (get (assoc this k default-value) k default-value)))
  (assoc [_ k v]
    (if (= (type v) RewriteMap)
      (RewriteMap.
        (z/up (z/replace (z-get zloc k) (z/node (.-zloc v)))))
      (RewriteMap.
        (-> zloc
            (z-assoc k v)
            (z-get k)
            z/left z/left
            z/append-newline
            z/up))))
  (dissoc [_ k]
    (RewriteMap.
      (loop [zloc (-> zloc (z/get k) z/left)]
        (prn "In loop " (z/string zloc))
        (cond
          (z/whitespace? zloc)
          (recur (z/right* (z/remove* zloc)))
          (z/whitespace-or-comment? zloc)
          (recur (z/right* zloc))
          :else
          (-> zloc z/remove*)))))
  (keys [_]
    (keys (z/sexpr zloc))))

(defmethod print-method RewriteMap
  [rm ^java.io.Writer writer]
  (.write writer (z/string (.-zloc rm))))

;; (extend-protocol cider.inlined-deps.fipp.v0v6v12.fipp.ednize/IEdn
;;   RewriteMap
;;   (-edn [rm]
;;     (z/string (.-zloc rm))))
;;
;; (extend-protocol cider.inlined-deps.fipp.v0v6v12.fipp.ednize/IOverride
;;   RewriteMap)

(defn rm-str
  [rm]
  (z/string (.-zloc rm)))

(defn root-str
  [rm]
  (z/root-string (.-zloc rm)))

(defn make-rm
  [zloc]
  (->RewriteMap (z/find zloc z/right (constantly true))))

(declare ->NodeMap)

(defn- maybe-to-node-map
  [x]
  (if (map? (n/sexpr x))
    (->NodeMap x)
    (n/sexpr x)))

(defn- maybe-to-node
  [x]
  (cond
    (= (type (->NodeMap nil)) (type x))
    (.-node x)

    (satisfies? rewrite-clj.node.protocols/Node x)
    x

    :else
    (n/token-node x)))

(defn- take-upto
  ([pred coll]
   (lazy-seq
     (when-let [s (seq coll)]
       (let [x (first s)]
         (cons x (if-not (pred x) (take-upto pred (rest s)))))))))

(defn- partition-by-kv
  [children]
  (lazy-seq
    (when (seq children)
      (let [[pre rst] (split-with n/whitespace-or-comment? children)
            k (first rst)
            [bet rst] (split-with n/whitespace-or-comment? (rest rst))
            v (first rst)]
        (cons (with-meta (concat (seq pre) (when k [k]) (seq bet) (when v [v]))
                         {:k k :v v})
              (partition-by-kv (rest rst)))))))

(comment
  (partition-by-kv (:children (parser/parse-file "test.edn")))
  (parser/parse-string "{:a,:b}")
  rewrite-clj.node.whitespace/comma?)

(defn- ordered-replace
  "Take a list of replacements to do when a predicate matches"
  [p reps coll]
  (first
    (reduce
      (fn [[new-coll reps] x]
        (if (and (p x) (seq reps))
          [(conj new-coll (first reps)) (rest reps)]
          [(conj new-coll x) reps]))
      [[] reps]
      coll)))

(def-map-type NodeMap [node]
  (get
    [this k default-value]
    (get
      (into {}
            (comp (filter (complement n/whitespace-or-comment?))
                  (partition-all 2)
                  (map (fn [[k v]]
                         [(n/sexpr k) (maybe-to-node-map v)])))
            (:children node))
      #_(into {}
            (comp
              (map (comp (juxt :k :v) meta))
              (filter first)
              (map (fn [[k v]]
                     [(n/sexpr k) (maybe-to-node-map v)])))
            (partition-by-kv (:children node)))
      k
      default-value))

  (assoc
    [_ k v]
    (->NodeMap
      (assoc node :children
             (let [{:keys [replaced? children]}
                   (reduce
                     (fn [acc child]
                       (let [add-child #(update %1 :children (fnil conj []) %2)]
                         (cond
                           (n/whitespace-or-comment? child)
                           (add-child acc child)

                           (and (:k? acc) (= (:k acc) k))
                           (add-child
                             (-> acc (dissoc :k? :k) (assoc :replaced? true))
                             (maybe-to-node v))

                           (not (:k? acc))
                           (add-child
                             (merge acc
                                    {:k? true :k (n/sexpr child)})
                             child)

                           :else
                           (add-child (merge acc {:k? false}) child))))
                     {:k? false}
                     (:children node))]
               (if replaced?
                 children
                 (concat
                   children
                   (let [other-kvs (partition-by-kv children)
                         k-count (count other-kvs)
                         prev (last other-kvs)]
                     (prn (meta (first children)))
                     (cond
                       (= 1 k-count)
                       (ordered-replace
                         (complement n/whitespace-or-comment?)
                         [(n/token-node k) (n/token-node v)]
                         (cons
                           (n/newline-node "\n")
                           (cons
                             (n/whitespace-node (apply str (repeat (-> children first meta :col dec) \space)))
                             (first other-kvs))))

                       (:k (meta prev))
                       (ordered-replace
                         (complement n/whitespace-or-comment?)
                         [(n/token-node k) (n/token-node v)]
                         prev)
                       :else
                       [(n/newline-node "\n") (n/token-node k) (n/whitespace-node " ") (n/token-node v)]))))))))

  (dissoc
    [this k]
    (->NodeMap
      (assoc node :children
             (apply concat
                    (filter
                      (fn [kv-seq]
                        (if-let [ka (:k (meta kv-seq))]
                          (not= k (n/sexpr ka))
                          true))
                      (partition-by-kv (:children node)))))))

  (keys
    [_]
    (sequence (comp (filter (complement n/whitespace-or-comment?))
                    (partition-all 2)
                    (map first)
                    (map maybe-to-node-map))
              (:children node))))

(comment
  (def root (make-rm (z/of-file "deps.edn"))))

(defn nm-string
  [nm]
  (n/string (.-node nm)))

(comment
  (let [root (->RewriteMap (z/of-file "deps.edn"))]
    (rm-str
      (assoc-in root
                [:deps 'foo.bar/bar]
                (get-in root [:deps 'potemkin/potemkin])))
    )
  ;; TODO: This fails, need to handle `nil` in the `get` of RewriteMap
  (z/get (let [zloc (.-zloc (get root :a))]
           (if (nil? (z/sexpr zloc))
             (z/replace zloc {})
             zloc)) :b)
  ;; blugh
  (println (-> root :a :b))

  (n/string (assoc (->NodeMap (parser/parse-file "test.edn")) :c 20))
  (nm-string (assoc-in
              (->NodeMap (parser/parse-file "deps.edn"))
              [:deps 'potemkin/potemkin :blah]
              "20"))

  (n/string (dissoc (:deps (->NodeMap (parser/parse-file "deps.edn"))) 'potemkin/potemkin))

  (nm-string (update (->NodeMap (parser/parse-file "deps.edn")) :deps dissoc 'rewrite-clj/rewrite-clj))

  (nm-string (update (->NodeMap (parser/parse-file "deps.edn")) :paths conj "foo"))

  (do (:children (dissoc (:deps (->NodeMap (parser/parse-file "deps.edn"))) 'potemkin/potemkin))
      nil)
  (n/string (dissoc (->NodeMap (parser/parse-file "deps.edn")) :deps))

  (n/coerce 'foo)
  (n/keyword-node :foo)

  (-> (parser/parse-file "test.edn")
      ;; (update :children vec)
      ;; (assoc-in [:children 0 :k] :b)
      ;; (update-in [:children])
      (assoc :tag :list)
      (n/string)
      )

  (update-in (parser/parse-file "test.edn") )

  (-> (.-zloc (:deps root))
      (z/get 'rewrite-clj/rewrite-clj)
      (z/left)
      (z/remove)
      (z/remove)
      z/root-string)

  (println (update root :deps dissoc 'potemkin/potemkin))
  (println (update root :deps dissoc 'rewrite-clj/rewrite-clj))

  (-> (.-zloc (:deps root))
      (z/get 'rewrite-clj/rewrite-clj)
      z/left
      (z/remove)
      (z/remove)
      z/root-string)

  (println (dissoc root :deps))
  (println (dissoc (make-rm (z/of-file "test.edn")) :a))

  (root-str (-> root (assoc-in [:deps 'foo.bar/baz] {:mvn/version "0.4.1"})))
  (-> (z/of-file "deps.edn")
      (->RewriteMap)
      (get :deps)
      type
      ))
