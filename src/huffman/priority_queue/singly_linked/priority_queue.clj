(ns huffman.priority-queue.singly-linked.priority-queue
  (:require [huffman.priority-queue.node-list :as nl])
  (:import (clojure.lang IPersistentStack ISeq)))

(declare new-priority-queue seq-queue push-queue peek-priority? pop-priority?
         equiv)

(deftype Priority-Queue [root size priority-comparator]
  ISeq
  (seq [q] (seq-queue q))

  (empty [q] (new-priority-queue priority-comparator))
  (count [q] size)
  (cons [q [p e :as priority-element-pair]] (push-queue q p e))
  (equiv [q1 q2] (equiv q1 q2))

  (first [q] (peek-priority? q))
  (next [q] (second (pop-priority? q)))
  (more [q] (or (next q)
                (empty q)))

  IPersistentStack
  (peek [q] (peek-priority? q))
  (pop [q] (or (next q)
               (throw (IllegalStateException. "Can't pop empty priority queue using pop. Use peek-priority? to
               return nil when peeking an empty queue.")))))

(defn new-priority-queue [priority-comparator]
  (->Priority-Queue nil 0 priority-comparator))

(defn new-minimum-priority-queue []
  (new-priority-queue <))

(defn new-maximum-priority-queue []
  (new-priority-queue >))

(defn queue-empty? [^Priority-Queue queue]
  (nil? (.root queue)))

; TODO: Find a way to generalize
(defn- alter-size [^Priority-Queue queue, f]
  (->Priority-Queue (.root queue) (f (.size queue)) (.priority_comparator queue)))

(defn- alter-root [^Priority-Queue queue, f]
  (->Priority-Queue (f (.root queue)) (.size queue) (.priority_comparator queue)))

(defn push-queue
  "Pushes the value with a given priority into the queue."
  [^Priority-Queue queue, priority, value]
  (when-not value
    (throw (RuntimeException. (str "Pushed values must be truthy!: " (pr-str value)))))

  (-> queue
      (alter-root #(nl/push-new-node % priority value (.priority_comparator queue)))
      (alter-size inc)))

(defn peek-priority?
  "Returns either a pair of [priority highest-priority-item], or nil if the queue is empty.
  If multiple items have the same priority, the first pushed item is returned."
  [^Priority-Queue queue]
  (when-not (queue-empty? queue)
    (nl/peek-root (.root queue))))

(defn pop-priority?
  "Returns either a pair of [highest-priority rest-queue], or nil if the queue is empty."
  [^Priority-Queue queue]
  (when-let [popped (peek-priority? queue)]
    [popped
     (-> queue
         (alter-root nl/remove-priority)
         (alter-size dec))]))

(defn seq-queue [^Priority-Queue queue]
  (when (and queue (not (queue-empty? queue)))
    (loop [rest-queue queue
           acc '()]
      (if-let [[popped remaining] (pop-priority? rest-queue)]
        (recur remaining (cons popped acc))
        acc))))

(defn equiv [^Priority-Queue q1, ^Priority-Queue q2]
  (and (= (.size q1) (.size q2))
       (= (.root q1) (.root q2))))