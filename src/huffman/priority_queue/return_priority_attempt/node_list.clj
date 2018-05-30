(ns huffman.priority-queue.return-priority-attempt.node-list)

(defrecord Node [priority bucket child])

(defn new-node [priority value]
  (->Node priority [value] nil))

(defn- add-value-to-node [node value]
  (update node :bucket conj value))

(defn- set-child-of-node [node child]
  (assoc node :child child))

; TODO Add equality-f as well?
(defn push-new-node [root priority value priority-comparator]
  (let [node (new-node priority value)]
    ((fn rec [{n-pri :priority, n-child :child :as cur-node}]
       (cond
         (nil? cur-node) node
         (priority-comparator priority n-pri) (set-child-of-node node cur-node)
         (= priority n-pri) (add-value-to-node cur-node value)
         :else (set-child-of-node cur-node (rec n-child))))
     root)))

(defn- has-n-values? [node n]
  (>= (count (:bucket node)) n))

(defn remove-priority [root]
  (if (has-n-values? root 2)
    (update root :bucket subvec 1)
    (:child root)))

(defn peek-root
  "Returns a pair of [priority highest-priority-item], or nil if the node has no values."
  [root]
  (when (has-n-values? root 1)
    (let [{:keys [bucket priority]} root]
      [priority (first bucket)])))