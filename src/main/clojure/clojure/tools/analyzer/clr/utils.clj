;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.analyzer.clr.utils
  (:require [clojure.reflect :as reflect]
            [clojure.string :as s]
            [clojure.tools.analyzer.clr.stubs :refer [lru]]    ;;; #_[clojure.core.memoize :refer [lru]]
            [clojure.clr.io :as io])                   ;;; clojure.java.io
  (:import (clojure.lang RT Symbol Var)
                                                      ;;; (org.objectweb.asm Type)
                                                      ;;; (java.io File)
           ))                                         ;;; (java.net URL)

(defn ^:private type-reflect
  [typeref & options]
  (apply reflect/type-reflect typeref
         :reflector (reflect/->ClrReflector nil)      ;;; (reflect/->JavaReflector (RT/baseLoader))
         options))

(defn specials [c]
  (case c
    "byte"    Byte         ;;; Byte/TYPE
    "boolean" Boolean      ;;; Boolean/TYPE
    "char"    Char         ;;; Character/TYPE
    "int"     Int32        ;;; Integer/TYPE
    "long"    Int64        ;;; Long/TYPE
    "float"   Single       ;;; Float/TYPE
    "double"  Double       ;;; Double/TYPE
    "short"   Int16        ;;; Short/TYPE
    "void"    System.Void  ;;; Void/TYPE
    "object"  Object       ;;; DM: Added
	"decimal" Decimal      ;;; DM: Added
	"sbyte"   SByte        ;;; DM: Added
	"ushort"  UInt16       ;;; DM: Added
	"uint"    UInt32       ;;; DM: Added
	"ulong"   UInt64       ;;; DM: Added
    nil))

(defn special-arrays [c]
  (case c
    "bytes"    |System.Byte[]|             ;;; (Class/forName "[B")
    "booleans" |System.Boolean[]|          ;;; (Class/forName "[Z")
    "chars"    |System.Char[]|             ;;; (Class/forName "[C")
    "ints"     |System.Int32[]|            ;;; (Class/forName "[I")
    "longs"    |System.Int64[]|            ;;; (Class/forName "[J")
    "floats"   |System.Single[]|           ;;; (Class/forName "[F")
    "doubles"  |System.Double[]|           ;;; (Class/forName "[D")
    "shorts"   |System.Int16[]|            ;;; (Class/forName "[S")
    "objects"  |System.Object[]|           ;;; (Class/forName "[Ljava.lang.Object;")
	"sbytes"   |System.SByte[]|            ;;;  DM: Added 
	"ushorts"  |System.Int16[]|            ;;;  DM: Added
	"uints"    |System.Int32[]|            ;;;  DM: Added
	"ulongs"   |System.Int64[]|            ;;;  DM: Added
	"decimals" |System.Decimal[]|          ;;;  DM: Added
    nil))

(defmulti ^Type -maybe-class class)        ;;; ^Class

(def ^Type maybe-class                     ;;; ^Class
  "Takes a Symbol, String or Class and tires to resolve to a matching Class"
  (lru (fn [x] (-maybe-class x))))

(defn array-class [element-type]
  (RT/classForNameE
   (str (-> element-type maybe-class .FullName (.Replace \/ \.)) "[]")         ;;; .getName  .replace
   #_(str "[" (-> element-type
              maybe-class
              Type/getType
              .getDescriptor
              (.replace \/ \.)))))

(defn maybe-class-from-string [s]
  (try
    (RT/classForNameE s)
    (catch Exception _
      (if-let [maybe-class ((ns-map *ns*) (symbol s))]
        (when (class? maybe-class)
          maybe-class)))))

(defmethod -maybe-class :default [_] nil)
(defmethod -maybe-class Type [c] c)                 ;;; Class
(defmethod -maybe-class String [s]
  (maybe-class (symbol s)))

(defmethod -maybe-class Symbol [sym]
  (when-not (namespace sym)
    (let [sname (name sym)
          snamec (count sname)]
      (if-let [base-type (and (.EndsWith sname "<>")                          ;;; .endsWith
                              (maybe-class (subs sname 0 (- snamec 2))))]
        (array-class base-type)
        (if-let [ret (or (specials sname)
                         (special-arrays sname))]
          ret
          (maybe-class-from-string sname))))))

(def primitive?
  "Returns non-nil if the argument represents a primitive Class other than Void"
  #{Double Char Byte Boolean SByte Decimal                                                  ;;; Double/TYPE Character/TYPE Byte/TYPE Boolean/TYPE
    Int16 Single Int64 Int32 UInt16 UInt64 UInt32})                                         ;;; Short/TYPE Float/TYPE Long/TYPE Integer/TYPE})

(def ^:private convertible-primitives            ;;; TODO: DM: Really need to see where this is used and fix it
  "If the argument is a primitive Class, returns a set of Classes
   to which the primitive Class can be casted"
  {Int32   #{Int32 Int64 Int16 Byte SByte}  ;;; Integer/TYPE   #{Integer Long/TYPE Long Short/TYPE Byte/TYPE}
   Single  #{Single Double}                 ;;; Float/TYPE     #{Float Double/TYPE}
   Double  #{Double Single}                 ;;; Double/TYPE    #{Double Float/TYPE}
   Int64   #{Int64 Int32 Int16 Byte}        ;;; Long/TYPE      #{Long Integer/TYPE Short/TYPE Byte/TYPE}
   Char    #{Char}                          ;;; Character/TYPE #{Character}
   Int16   #{Int16}                         ;;; Short/TYPE     #{Short}
   Byte    #{Byte}                          ;;; Byte/TYPE      #{Byte}
   Boolean #{Boolean}                       ;;; Boolean/TYPE   #{Boolean}
   UInt32  #{Int32 Int64 Int16 Byte SByte}  ;;; DM: Added
   UInt64  #{Int64 Int32 Int16 Byte}        ;;; DM: Added
   UInt16  #{Int16}                         ;;; DM: Added
   SByte   #{SByte}                         ;;; DM: Added
   Decimal #{Decimal}                       ;;; DM: Added       
   System.Void    #{System.Void}})          ;;; Void/TYPE      #{Void}

(defn ^Type box                           ;;; ^Class
  "If the argument is a primitive Class, returns its boxed equivalent,
   otherwise returns the argument"
  [c] 
  #_({Integer/TYPE   Integer
    Float/TYPE     Float
    Double/TYPE    Double
    Long/TYPE      Long
    Character/TYPE Character
    Short/TYPE     Short
    Byte/TYPE      Byte
    Boolean/TYPE   Boolean
    Void/TYPE      Void}
   c c)
   c)

(defn ^Type unbox                                                          ;;; ^Class
  "If the argument is a Class with a primitive equivalent, returns that,
   otherwise returns the argument"
  [c]
  #_({Integer   Integer/TYPE,
    Long      Long/TYPE,
    Float     Float/TYPE,
    Short     Short/TYPE,
    Boolean   Boolean/TYPE,
    Byte      Byte/TYPE,
    Character Character/TYPE,
    Double    Double/TYPE,
    Void      Void/TYPE}
   c c)
   c)

(defn numeric?
  "Returns true if the given class is numeric"
  [c]
  (when c
      (clojure.lang.Util/IsNumeric ^Type c)))            ;;; (.isAssignableFrom Number (box c))

(defn subsumes?
  "Returns true if c2 is subsumed by c1"
  [c1 c2]
  (let [c1 (maybe-class c1)
        c2 (maybe-class c2)]
    (and (not= c1 c2)
         (or (and (not (primitive? c1))
                  (primitive? c2))
             (.IsAssignableFrom c2 c1)))))               ;;; .isAssignableFrom 

(defn convertible?
  "Returns true if it's possible to convert from c1 to c2"
  [c1 c2]
  (let [c1 (maybe-class c1)
        c2 (maybe-class c2)]
    (if (nil? c1)
      (not (primitive? c2))
      (or
       (= c1 c2)
       (.IsAssignableFrom c2 c1)                         ;;; .isAssignableFrom 
       (and (primitive? c2)
            ((convertible-primitives c2) c1))))))

(def wider-than
  "If the argument is a numeric primitive Class, returns a set of primitive Classes
   that are narrower than the given one"
  {Int64   #{Int32 UInt32 Int16 UInt16 Byte SByte}            ;;; Long/TYPE    #{Integer/TYPE Short/TYPE Byte/TYPE}
   Int32   #{Int16 UInt16 Byte SByte}                         ;;; Integer/TYPE #{Short/TYPE Byte/TYPE}
   Single  #{Int32 UInt32 Int16 UInt16 Byte SByte}            ;;; Float/TYPE   #{Integer/TYPE Short/TYPE Byte/TYPE Long/TYPE}
   Double  #{Int32 UInt32 Int16 UInt16 Byte SByte Single}     ;;; Double/TYPE  #{Integer/TYPE Short/TYPE Byte/TYPE Long/TYPE Float/TYPE}
   Int16   #{Byte SByte}                                      ;;; Short/TYPE   #{Byte/TYPE}
   UInt64  #{Int32 UInt32 Int16 UInt16 Byte SByte}            ;;; DM: Added
   UInt32  #{Int16 UInt16 Byte SByte}                         ;;; DM: Added
   UInt16  #{Byte SByte}                                      ;;; DM: Added
   Decimal #{}                                                ;;; DM: Added
   Byte    #{}})                                              ;;; Byte/TYPE    #{}

(defn wider-primitive
  "Given two numeric primitive Classes, returns the wider one"
  [from to]
  (if ((wider-than from) to)
    from
    to))

(defn wider-tag*
  "Given two Classes returns the wider one"
  [from to]
  (if (not= from to)
    (if (primitive? from)
      (if (primitive? to)
        (wider-primitive from to)
        (or (and (numeric? from)
                 (numeric? to)
                 to)
            ((convertible-primitives from) to)))
      (if (primitive? to)
        (or (and (numeric? from)
                 (numeric? to)
                 from)
            ((convertible-primitives to) from))
        (if (convertible? from to)
          to
          (when (convertible? to from)
            from))))
    from))

(defn wider-tag
  "Given a collection of Classes returns the wider one"
  [tags]
  (let [tags* (filter identity tags)
        wider (loop [wider (first tags*) tags* (rest tags*)]
                (if (seq tags*)
                  (if-let [t (wider-tag* wider (first tags*))]
                    (recur t (rest tags*)))
                  wider))]
    (when (or (= tags* tags)
              (not (primitive? wider)))
      wider)))

(defn name-matches?
  [member]
  (let [member-name (str member)
        i (.LastIndexOf member-name ".")                  ;;; .lastIndexOf
        member-name* (when (pos? i)
                       (str (s/replace (subs member-name 0 i) "-" "_") (subs member-name i)))
        member-name** (s/replace member-name "-" "_")
        member-name*** (munge member-name)]
    (fn [name]
      (let [name (str name)]
        (or (= member-name name)
            (= member-name* name)
            (= member-name** name)
            (= member-name*** name))))))

(def object-members
  (:members (type-reflect Object)))

(def members*
  (lru (fn ([class]
             (into object-members
                   (remove (fn [{:keys [flags]}]
                             (not-any? #{:public :protected} flags))
                           (-> (maybe-class class)
                             box
                             (type-reflect :ancestors true)
                             :members)))))))

(defn members
  ([class] (members* class))
  ([class member]
     (when-let [members (filter #((name-matches? member) (:name %))
                                (members* class))]
       members)))

(defn static-members [class f]
  (when-let [members (members class f)]
    (when-let [statics (filter (comp :static :flags) members)]
      statics)))

(defn instance-members [class f]
  (when-let [members (members class f)]
    (when-let [i-members (remove (comp :static :flags) members)]
      i-members)))

(defn static-methods [class method argc]
  (filter #(= argc (count (:parameter-types %)))
          (filter :return-type (static-members class method))))

(defn instance-methods [class method argc]
  (filter #(= argc (count (:parameter-types %)))
          (filter :return-type (instance-members class method))))

(defn static-field [class f]
  (when-let [statics (static-members class f)]
    (when-let [[member] (filter (every-pred (comp nil? seq :parameter-types)
                                            (comp nil? :return-type))
                                statics)]
      member)))

(defn instance-field [class f]
  (when-let [i-members (instance-members class f)]
    (when-let [[member] (filter (every-pred (comp nil? seq :parameter-types)
                                            (comp nil? :return-type))
                                i-members)]
      member)))

(defn static-method [class method]
  (first (static-methods class method 0)))

(defn instance-method [class method]
  (first (instance-methods class method 0)))

(defn prim-or-obj
  "If the given Class is a primitive, returns that Class, otherwise returns Object"
  [tag]
  (if (and tag (primitive? tag))
    tag
    Object))                           ;;; java.lang.Object

(defn prim-interface [tags]
  (when (some primitive? tags)
    (let [sig (apply str (mapv #(.ToUpper (subs (.Name ^Type %) 0 1)) tags))]    ;;; .toUpperCase  .getSimpleName ^Class
      (maybe-class (str "clojure.lang.IFn$" sig)))))

(defn tag-match? [arg-tags meth]
  (every? identity (map convertible? arg-tags (:parameter-types meth))))

(defn try-best-match
  "Given a vector of arg tags and a collection of methods, tries to return the
   subset of methods that match best the given tags"
  [tags methods]
  (let [o-tags (mapv #(or (maybe-class %) Object) tags)]
    (if-let [methods (or (seq (filter
                               #(= o-tags (mapv maybe-class (:parameter-types %))) methods))
                         (seq (filter #(tag-match? tags %) methods)))]
      (reduce (fn [[prev & _ :as p] next]
                (let [prev-params (mapv maybe-class (:parameter-types prev))
                      next-params (mapv maybe-class (:parameter-types next))
                      prev-ret    (maybe-class (:return-type prev))
                      next-ret    (maybe-class (:return-type next))
                      prev-decl   (maybe-class (:declaring-class prev))
                      next-decl   (maybe-class (:declaring-class next))]
                  (cond
                  (not prev)
                  [next]
                  (= prev-params next-params)
                  (cond
                   (= prev-ret next-ret)
                   (cond
                    (.IsAssignableFrom prev-decl next-decl)  ;;; .isAssignableFrom
                    [next]
                    (.IsAssignableFrom next-decl prev-decl)  ;;; .isAssignableFrom
                    p
                    :else
                    (conj p next))
                   (.IsAssignableFrom prev-ret next-ret)  ;;; .isAssignableFrom
                   [next]
                   (.IsAssignableFrom next-ret prev-ret)  ;;; .isAssignableFrom
                   p
                   :else
                   (conj p next))
                  (and (some true? (map subsumes? next-params prev-params))
                       (not-any? true? (map subsumes? prev-params next-params)))
                  [next]
                  :else
                  (conj p next)))) [] methods)
      methods)))

(defn source-path [x]
  #_(if (instance? File x)
    (.getAbsolutePath ^File x)
    (str x))
	(if (instance? System.IO.FileInfo x)   
	  (.DirectoryName x)
	  (str x)))

(defn ns->relpath [s]
  (str (s/replace (munge (str s)) \. \/) ".clj"))

(defn ns-resource [ns]
  #_(let [f (ns->relpath ns)]
   (cond
    (instance? File f) f
    (instance? URL f) f
    (re-find #"^file://" f) (URL. f)
    :else (io/resource f)))
	(ns->relpath ns))

(defn res-path [res]
  #_(if (instance? File res)
    (.getPath ^File res)
    (.getPath ^URL res))
	(.DirectoryName res))
