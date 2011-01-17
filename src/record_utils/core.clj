(ns record-utils.core
  "A bunch of utils for dealing with records"
  (:use [clojure.contrib.str-utils :only [re-gsub]]))

(defn- ctor-symbol
  "Returns dashed name for the record constructor
   ex: PersonOfInterest  make-person-of-interest"
  [type-name]
  (symbol
   (str "make" (re-gsub  #"[A-Z]"
                         #(format "-%s" (.toLowerCase  %))
                         (str type-name)))))

(defmacro make-record-ctor
  "Returns a new constructor accepting a map as record parameters"
  [type-name ctor-name fields]
  `(defn ~ctor-name
     [field-map#]
     (apply (fn [& [~@fields]] (new ~type-name ~@fields))
            (for [field# '~fields]
              (get field-map# (keyword field#))))))

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
