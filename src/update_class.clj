(ns update-class
  (:require [clojure.reflect :as r])
  (:import (clojure.reflect Constructor Method Field)
           (clojure.lang Symbol)))

; TODO: Cache and use result of r/reflect

(deftype Test-Type [a b c])
(defrecord Test-Record [x y z])

(defn things-of [^Class c, ^Class member-type]
  (filter #(instance? member-type %)
          (:members (r/reflect c))))

(defn constructors-of [^Class c]
  (things-of c Constructor))

(defn methods-of [^Class c]
  (things-of c Method))

(defn fields-of [^Class c]
  (things-of c Field))

(defn has-method? [^Class c, ^Symbol method-name]
  (some #(= (:name %) method-name)
        (methods-of c)))

(defn alter-field [^Object obj, field-key, f]
  (let [req'd-method 'getBasis
        obj-c (.getClass obj)]
    (when-not (has-method? obj-c req'd-method)
      (throw (IllegalArgumentException.
               (str "Class " obj-c " doesn't have required method " req'd-method))))

    (let [])))

