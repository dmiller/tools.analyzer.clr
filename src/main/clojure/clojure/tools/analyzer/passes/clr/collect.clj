;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.analyzer.passes.clr.collect
  (:require [clojure.tools.analyzer.ast :refer [update-children]]))

(def ^:private ^:dynamic *collects*)

(defn -register-constant
  [form tag type]
  (let [key {:form form
             :meta (meta form)
             :tag  tag}
        collects @*collects*]
    (or (:id ((:constants collects) key)) ;; constant already in the constant table
        (let [id (:next-id collects)]
          (swap! *collects* #(assoc-in (update-in % [:next-id] inc)
                                       [:constants key]
                                       {:id   id
                                        :tag  tag
                                        :val  form
                                        :type type}))
          id))))

(defmulti -collect-const    :op)
(defmulti -collect-callsite :op)

(defmethod -collect-const    :default [ast] ast)
(defmethod -collect-callsite :default [ast] ast)

(defmethod -collect-const :const
  [{:keys [val tag type] :as ast}]
  (if (and (not= type :nil)        ;; nil and true/false can be emitted as literals,
           (not= type :boolean)) ;; no need to put them on the constant table
    (let [id (-register-constant val tag type)]
      (assoc ast :id id))
    ast))

(defmethod -collect-const :def
  [ast]
  (let [id (-register-constant (:var ast) clojure.lang.Var :var)]
    (assoc ast :id id)))

(defmethod -collect-const :var
  [ast]
  (let [id (-register-constant (:var ast) clojure.lang.Var :var)]
    (assoc ast :id id)))

(defmethod -collect-const :the-var
  [ast]
  (let [id (-register-constant (:var ast) clojure.lang.Var :var)]
    (assoc ast :id id)))

(defmethod -collect-callsite :keyword-invoke
  [ast]
  (swap! *collects* #(update-in % [:keyword-callsites] conj (-> ast :keyword :form)))
  ast)

(defmethod -collect-callsite :protocol-invoke
  [ast]
  (swap! *collects* #(update-in % [:protocol-callsites] conj (-> ast :protocol-fn :var)))
  ast)

(defn merge-collects [ast]
  (merge ast (dissoc @*collects* :where :what :next-id :top-level?)))

;; collects constants and callsites in one pass
(defn -collect [ast collect-fn]
  (let [collects @*collects*
        collect? ((:where collects) (:op ast))

        ast (with-bindings ;; if it's a collection point, set up an empty constant/callsite frame
              (if collect? {#'*collects* (atom (merge collects
                                                      {:next-id            0
                                                       :constants          {}
                                                       :protocol-callsites #{}
                                                       :keyword-callsites  #{}}))}
                  {})
              (let [ast (-> ast (update-children #(-collect % collect-fn))
                           collect-fn)]
                (if collect?
                  (merge-collects ast)
                  ast)))]
        ast))


(defn collect-fns [what]
  (case what
    :constants    -collect-const
    :callsites    -collect-callsite
    nil))

(defn collect
  "Takes a map with:
   * :what        set of keywords describing what to collect, some of:
     ** :constants     constant expressions
     ** :callsites     keyword and protocol callsites
   * :where       set of :op nodes where to attach collected info
   * :top-level?  if true attach collected info to the top-level node

   Returns a function that does the takes an AST and returns an AST with the
   collected info."
  [{:keys [what top-level?] :as opts}]
  (fn this [ast]
    (binding [*collects* (atom (merge {:constants           {}
                                       :protocol-callsites #{}
                                       :keyword-callsites  #{}
                                       :where              #{}
                                       :what               #{}
                                       :next-id             0}
                                      opts))]
      (let [ast (-collect ast (apply comp (keep collect-fns what)))]
        (if top-level?
          (merge-collects ast)
          ast)))))
