(ns record-utils.core
  "A bunch of utils for dealing with records"
   (:use [clojure.contrib.string :only [lower-case]]))

(defn camel-to-dashed
  "Convert a name like 'BigBlueCar' to 'big-blue-car'."
  [s]
  (let [parts (drop-last (re-seq #"[A-Z]?[a-z]*" s))]
    (if (= 1 (count parts))
      (-> parts first lower-case)
      (->> parts
           (interpose "-")
           (apply str)
           lower-case))))

(defn- ctor-symbol
  "Returns dashed name for the record constructor
   ex: PersonOfInterest  make-person-of-interest"
  [type-name]
  (symbol
   (str "make-" (-> type-name str camel-to-dashed))))

(defmacro make-record-ctor
  "Returns a new constructor accepting a map as record parameters"
  [type-name ctor-name fields]
  `(defn ~ctor-name [{:keys ~fields}]
     (new ~type-name ~@fields)))

(defmacro prepare-def-record
  ([type-name fields]
     `(defrecord ~type-name ~fields))
  ([type-name fields & args]
     `(defrecord ~type-name ~fields ~@args)))

(defmacro def-record
  "Works just like defrecord, the only difference is that it takes a map as
   fields and will nullify missing fields"
  [type-name fields & args]
  `(do
     (prepare-def-record ~type-name ~fields ~@args)
     (make-record-ctor ~type-name
                       ~(ctor-symbol type-name)
                       ~fields)))
