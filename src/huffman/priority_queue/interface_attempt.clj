; Adapt to manually create new instances of Priority-Queue with each operation

(ns huffman.priority-queue.interface-attempt
  (:refer-clojure :exclude [seq])
  (:import (clojure.lang IPersistentCollection Seqable IPersistentStack)))

(declare new-priority-queue seq-queue push-queue peek-priority? pop-priority?)
(deftype Priority-Queue [root size priority-comparator]
  IPersistentStack
  (seq [q] (seq-queue q))

  (empty [q] (new-priority-queue (:priority-comparator q)))
  (count [q] (:size q))
  (cons [q [p e :as priority-element-pair]] (push-queue q p e))
  (equiv [q1 q2] (= q1 q2)) ; FIXME: Iterate nodes for speed? Check size prior?
  (peek [q] (peek-priority? q))
  (pop [q] (or (second (pop-priority? q))
               (throw (IllegalStateException. "Can't pop empty priority queue using pop. Use peek-priority? to
               return nil when peeking an empty queue.")))))

(defrecord Node [priority bucket child])

(defn new-node [priority value]
  (->Node priority [value] nil))

(defn- add-value-to-node [node value]
  (update node :bucket conj value))

(defn- set-child-of-node [node child]
  (assoc node :child child))

(defn new-priority-queue [priority-comparator]
  (->Priority-Queue nil 0 priority-comparator))

(defn new-minimum-priority-queue []
  (new-priority-queue <))

(defn new-maximum-priority-queue []
  (new-priority-queue >))

(defn queue-empty? [queue]
  (-> queue :root (nil?)))

(defn- push-new-node [queue priority value]
  (let [node (new-node priority value)
        higher-priority-than? (:priority-comparator queue)]
    (update queue :root
            (fn rec [{n-pri :priority, n-child :child :as root}]
              (cond
                (nil? root) node
                (higher-priority-than? priority n-pri) (set-child-of-node node root)
                (= priority n-pri) (add-value-to-node root value)
                :else (set-child-of-node root (rec n-child)))))))

(defn push-queue
  "Pushes the value with a given priority into the queue."
  [queue priority value]
  (when-not value
    (throw (RuntimeException. (str "Pushed values must be truthy!: " (pr-str value)))))

  (-> queue
      (push-new-node priority value)
      (update :size inc)))

(defn peek-priority?
  "Returns either the highest priority item, or nil if the queue is empty.
  If multiple items have the same priority, the first pushed item is returned."
  [queue]
  (when-not (queue-empty? queue)
    (-> queue :root :bucket (first))))

(defn- remove-priority [queue]
  (update queue :root
          (fn [root]
            (if (= (count (:bucket root)) 1)
              (:child root)
              (update root :bucket subvec 1)))))

(defn pop-priority?
  "Returns either a pair of [highest-priority rest-queue], or nil if the queue is empty."
  [queue]
  (when-let [popped (peek-priority? queue)]
    [popped
     (-> queue
         (remove-priority)
         (update :size dec))]))

(defn seq-queue [queue]
  (when (and queue (not (queue-empty? queue)))
    (loop [rest-queue queue
           acc []]
      (if-let [[popped remaining] (pop-priority? rest-queue)]
        (recur remaining (conj acc popped))
        acc))))
