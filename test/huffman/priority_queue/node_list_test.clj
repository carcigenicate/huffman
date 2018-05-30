(ns huffman.priority-queue.node-list-test
  (:require [clojure.test :refer :all])
  (:require [huffman.priority-queue.singly-linked.node-list :as nl]))

(deftest push-new-node-test
  (testing "Values are pushed properly."))

(deftest peek-root-test
  (testing "The highest priority is always returned"
    (let [new-test-q #(-> (nl/new-node 5 :five)
                          (nl/push-new-node 3 :three %)
                          (nl/push-new-node 4 :four %)
                          (nl/push-new-node 1 :one %)
                          (nl/push-new-node 2 :two %))
          l-q (new-test-q <)
          h-q (new-test-q >)]
      (are [q f r] (= (f q) r)
        l-q nl/peek-root [1 :one]
        h-q nl/peek-root [5 :five]))))

(deftest remove-priority-test
  (let [p-comp <
        test-node (-> (nl/new-node 0 :item)
                      (nl/push-new-node 1 :item2 p-comp)
                      (nl/push-new-node -1 :lowest p-comp))]
    (testing "Empty queues are handled properly"
      (is (nil? (nl/remove-priority nil))))

    (testing "The roots are removed properly"
      (are [f r] (= (f test-node) r)
        nl/peek-root
        [-1 :lowest]

        (comp nl/peek-root nl/remove-priority)
        [0 :item]

        (comp nl/peek-root nl/remove-priority nl/remove-priority)
        [1 :item2]))))

