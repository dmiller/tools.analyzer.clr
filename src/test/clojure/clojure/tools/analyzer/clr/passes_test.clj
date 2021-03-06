(ns clojure.tools.analyzer.clr.passes-test
  (:refer-clojure :exclude [macroexpand-1])
  (:require [clojure.tools.analyzer.ast :refer :all]
            [clojure.tools.analyzer.clr :as ana.clr]
            [clojure.tools.analyzer.env :as env]
            [clojure.test :refer [deftest is]]
            [clojure.set :as set]
            [clojure.tools.analyzer.passes.add-binding-atom :refer [add-binding-atom]]
            [clojure.tools.analyzer.passes.collect-closed-overs :refer [collect-closed-overs]]
            [clojure.tools.analyzer.clr.core-test :refer [ast ast1 e f f1]]
            [clojure.tools.analyzer.passes.clr.emit-form
             :refer [emit-form emit-hygienic-form]]
            [clojure.tools.analyzer.passes.clr.validate :as v]
            [clojure.tools.analyzer.passes.clr.annotate-tag :refer [annotate-tag]]
            [clojure.tools.analyzer.passes.clr.clear-locals :refer [clear-locals]]
            [clojure.tools.analyzer.passes.clr.infer-tag :refer [infer-tag]]
            [clojure.tools.analyzer.passes.clr.annotate-branch :refer [annotate-branch]]
            [clojure.tools.analyzer.passes.clr.annotate-methods :refer [annotate-methods]]
            [clojure.tools.analyzer.passes.clr.annotate-loops :refer [annotate-loops]]
            [clojure.tools.analyzer.passes.clr.fix-case-test :refer [fix-case-test]]
            [clojure.tools.analyzer.passes.clr.analyze-host-expr :refer [analyze-host-expr]]
            [clojure.tools.analyzer.passes.clr.classify-invoke :refer [classify-invoke]])
  (:import (clojure.lang Keyword Var Symbol AFunction
                         PersistentVector PersistentArrayMap PersistentHashSet ISeq)
           System.Text.RegularExpressions.Regex))                                          ;;; java.util.regex.Pattern

(defn validate [ast]
  (env/with-env (ana.clr/global-env)
    (v/validate ast)))

(deftest collect-test
  (let [c-test (-> (ast1 (let [a 1 b 2] (fn [x] (fn [] [+ (:foo {}) x a]))))
                 :body :ret)]
    (is (= '#{a__#0} (-> c-test :closed-overs keys set)))
    (is (set/subset? #{{:form :foo
                        :tag  Keyword
                        :meta nil}
                       {:form #'+
                        :meta (meta #'+)
                        :tag  clojure.lang.Var}
                       {:form {}
                        :tag  PersistentArrayMap
                        :meta nil}}
                     (-> c-test :methods first :body :ret :constants keys set))) ;; it registers metadata too (line+col info)
    (is (= '#{a__#0 x__#0} (-> c-test :methods first :body :ret :closed-overs keys set)))))

(deftest emit-form-test
  (is (= '(monitor-enter 1) (emit-form (ast (monitor-enter 1)))))
  (is (= '(monitor-exit 1) (emit-form (ast (monitor-exit 1)))))
  (is (= '(clojure.core/import* "java.lang.String")
         (emit-form (validate (ast (clojure.core/import* "java.lang.String"))))))
  (is (= '(var clojure.core/+) (emit-form (ast #'+))))
  (is (= '(:foo {}) (emit-form (ast (:foo {})))))
  (is (= '(try 1 (catch Exception e nil))
         (emit-form (ana.clr/analyze '(try 1 (catch Exception e))))))
  (is (= '(try 1 (catch Exception e nil))
         (emit-form (ana.clr/analyze '(try 1 (catch Exception e)))
                    {:qualifed-symbols true})))
  (is (= '(f [] 1) (emit-form (ast (f [] 1))))))

(deftest annotate-branch-test
  (let [i-ast (annotate-branch (ast (if 1 2 3)))]
    (is (:branch? i-ast))
    (is (= true (-> i-ast :test :test?)))
    (is (= true (-> i-ast :then :path?)))
    (is (= true (-> i-ast :else :path?))))

  (let [fn-ast (prewalk (ast (fn ([]) ([x]))) annotate-branch)]
    (is (every? :path? (-> fn-ast :methods))))

  (let [r-ast (prewalk (ast (reify Object (toString [this] x))) annotate-branch)]
    (is (every? :path? (-> r-ast :methods))))

  (let [c-ast (-> (ast (case 1 0 0 2 2 1)) :body :ret (prewalk annotate-branch))]
    (is (:branch? c-ast))
    (is (= true (-> c-ast :test :test?)))
    (is (= true (-> c-ast :default :path?)))
    (is (every? :path? (-> c-ast :thens)))))

(deftest clear-locals-test
  (let [f-expr (-> (ast (fn [x] (if x x x) x (if x (do x x) (if x x x))))
                 (prewalk (comp annotate-branch add-binding-atom))
                 clear-locals :methods first :body)]
    (is (= true (-> f-expr :statements first :then :to-clear? nil?)))
    (is (= true (-> f-expr :statements first :else :to-clear? nil?)))
    (is (= true (-> f-expr :statements second :to-clear? nil?)))
    (is (= true (-> f-expr :ret :then :statements first :to-clear? nil?)))
    (is (= true (-> f-expr :ret :then :ret :to-clear?)))
    (is (= true (-> f-expr :ret :else :then :to-clear?)))
    (is (= true (-> f-expr :ret :else :else :to-clear?))))
  (let [f-expr (-> (ast (fn [x] (loop [a x] (if 1 x (do x (recur x))))))
                 (prewalk (comp annotate-branch annotate-loops add-binding-atom))
                 (collect-closed-overs {:what  #{:closed-overs}
                                        :where #{:fn :loop}
                                        :top-level? false})
                 clear-locals :methods first :body :ret)]
    (is (= true (-> f-expr :bindings first :init :to-clear? nil?)))
    (is (= true (-> f-expr :body :ret :then :to-clear?)))
    (is (= true (-> f-expr :body :ret :else :statements first :to-clear? nil?)))
    (is (= true (-> f-expr :body :ret :else :ret :exprs first :to-clear? nil?))))
  (let [f-expr (-> (ast (loop [] (let [a 1] (loop [] a)) (recur)))
                 (prewalk (comp annotate-branch annotate-loops add-binding-atom))
                 (collect-closed-overs {:what  #{:closed-overs}
                                        :where #{:loop}
                                        :top-level? false})
                 clear-locals
                 :body :statements first :body :ret :body :ret)]
    (is (= true (-> f-expr :to-clear?))))
  (let [f-expr (-> (ast (loop [] (let [a 1] (loop [] (if 1 a (recur)))) (recur)))
                 (prewalk (comp annotate-branch annotate-loops add-binding-atom))
                 (collect-closed-overs {:what  #{:closed-overs}
                                        :where #{:loop}
                                        :top-level? false})
                 clear-locals
                 :body :statements first :body :ret :body :ret :then)]
    (is (= true (-> f-expr :to-clear?))))
  (let [f-expr (-> (ast (let [a 1] (loop [] (let [b 2] (loop [] (if 1 [a b] (recur)))) (recur))))
                 (prewalk (comp annotate-branch annotate-loops add-binding-atom))
                 (collect-closed-overs {:what  #{:closed-overs}
                                        :where #{:loop}
                                        :top-level? false})
                 clear-locals
                 :body :ret :body :statements first :body :ret :body :ret :then :items)]
    (is (= true (-> f-expr first :to-clear? nil?)))
    (is (= true (-> f-expr second :to-clear?))))
  (let [f-expr (-> (ast (let [a 1] (loop [] (if 1 a) (recur))))
                 (prewalk (comp annotate-branch annotate-loops add-binding-atom))
                 (collect-closed-overs {:what  #{:closed-overs}
                                        :where #{:loop}
                                        :top-level? false})
                 clear-locals
                 :body :ret :body :statements first :then)]
    (is (= true (-> f-expr :to-clear? nil?))))
  (let [f-expr (-> (ast (let [a 1] (loop [] (let [x (if 1 a)]) (recur))))
                 (prewalk (comp annotate-branch annotate-loops add-binding-atom))
                 (collect-closed-overs {:what  #{:closed-overs}
                                        :where #{:loop}
                                        :top-level? false})
                 clear-locals
                 :body :ret :body :statements first :bindings first :init :then)]
    (is (= true (-> f-expr :to-clear? nil?)))))

(deftest fix-case-test-test
  (let [c-ast (-> (ast (case 1 1 1)) add-binding-atom (prewalk fix-case-test))]
    (is (= true (-> c-ast :body :ret :test :atom deref :case-test)))))

(deftest annotate-tag-test
  (is (= PersistentVector (-> {:op :const :form [] :val []} annotate-tag :tag)))
  (is (= PersistentVector (-> (ast []) annotate-tag :tag)))
  (is (= PersistentArrayMap(-> (ast {}) annotate-tag :tag)))
  (is (= PersistentHashSet (-> (ast #{}) annotate-tag :tag)))
  (is (= Type (-> {:op :const :type :class :form Object :val Object}                   ;;; Class
                 annotate-tag :tag)))
  (is (= String (-> (ast "foo") annotate-tag :tag)))
  (is (= Keyword (-> (ast :foo) annotate-tag :tag)))
  (is (= Char (-> (ast \f) annotate-tag :tag)))               ;;; Character/TYPE 
  (is (= Int64 (-> (ast 1) annotate-tag :tag)))               ;;; Long/TYPE
  (is (= Regex (-> (ast #"foo") annotate-tag :tag)))          ;;; Pattern
  (is (= Var (-> (ast #'+)  annotate-tag :tag)))
  (is (= Boolean (-> (ast true) annotate-tag :tag)))
  (let [b-ast (-> (ast (let [a 1] a)) add-binding-atom
                 (postwalk annotate-tag))]
    (is (= Int64 (-> b-ast :body :ret :tag)))))               ;;; Long/TYPE

(deftest classify-invoke-test
  (is (= :keyword-invoke (-> (ast (:foo {})) classify-invoke :op)))
  (is (= :protocol-invoke (-> (ast (f nil)) classify-invoke :op)))
  (is (= :instance? (-> (ast (instance? String ""))
                      (prewalk analyze-host-expr) classify-invoke :op)))
  (is (= :prim-invoke (-> (ast (f1 1)) (prewalk infer-tag) classify-invoke :op))))

(deftest annotate-methods-test
  (let [r-ast (-> (ast ^:foo (reify Object (toString [_] ""))) (prewalk annotate-methods))]
    (is (= 'toString (-> r-ast :expr :methods first :name)))
    (is (= [] (-> r-ast :expr :methods first :params)))
    (is (= '_ (-> r-ast :expr :methods first :this :name)))))

;; TODO: test primitives, tag matching, throwing validation, method validation
(deftest validate-test
  (is (= Exception (-> (ast (try (catch Exception e)))
                     (prewalk (comp validate analyze-host-expr)) :catches first :class :val)))
  (is (-> (ast (set! *warn-on-reflection* true)) validate))
  (is (= true (-> (ast (String. "foo")) (postwalk (comp validate annotate-tag analyze-host-expr))
              :validated?)))

  (let [s-ast (-> (ast (Int32/Parse "7")) (prewalk annotate-tag) analyze-host-expr validate)]   ;;; Integer/parseInt
    (is (:validated? s-ast))
    (is (= Int32 (:tag s-ast)))                         ;;; Integer/TYPE
    (is (= [String] (mapv :tag (:args s-ast)))))

  (let [i-ast (-> (ast (.GetHashCode "7")) (prewalk annotate-tag) analyze-host-expr validate)]           ;;; .hashCode 
    (is (:validated? i-ast))
    (is (= Int32 (:tag i-ast)))                              ;;; Integer/TYPE
    (is (= [] (mapv :tag (:args i-ast))))
    (is (= String (:class i-ast))))

  (is (= true (-> (ast (import java.lang.String)) (prewalk validate) :ret :validated?))))

;; we need all or most those passes to perform those tests
(deftest all-passes-test
  (let [t-ast (ast1 (let [a 1
                          b 2
                          c (str a)
                          d (Convert/ToInt32 c b)]                                   ;;; Integer/parseInt
                      (Int32/ParseInt c)))]                                    ;;; (Integer/getInteger c d)
    (is (= Int32 (-> t-ast :body :tag)))                                              ;;; Integer
    (is (= Int32 (-> t-ast :tag)))                                                    ;;; Integer
    (is (= Int64 (->> t-ast :bindings (filter #(= 'a (:form %))) first :tag)))         ;;; Long/TYPE
    (is (= String (->> t-ast :bindings (filter #(= 'c (:form %))) first :tag)))
    (is (= Int32 (->> t-ast :bindings (filter #(= 'd (:form %))) first :tag))))       ;;; Integer/TYPE
  (is (= System.Void (:tag (ast1 (.WriteLine System/Console "foo")))))                ;;; Void/TYPE   .println System/out

  (is (= String (-> (ast1 String) :val)))
  (is (= 'String (-> (ast1 String) :form)))
(is (= PersistentVector (-> (ast1 '[]) :tag)))
(is (= ISeq (-> (ast1 '()) :tag)))

  (let [d-ast (ast1 (Double/IsInfinite 2))]                     ;;; Double/isInfinite
    (is (= Boolean (-> d-ast :tag)))                            ;;; Boolean/TYPE
    (is (= Double (->> d-ast :args first :tag)))))              ;;; Double/TYPE

;; checks for specific bugs that have surfaced
(deftest annotate-case-loop
  (is (ast1 (loop [] (case 1 :a (recur) :b 42)))))

(deftest var-tag-inference
  (let [ast (ana.clr/analyze '(def a "foo")
                             (ana.clr/empty-env)
                             {:passes-opts {:infer-tag/level :global}})]
    (is (= String (-> ast :var meta :tag)))))

(deftest validate-handlers
  ;; test for tanal-24, without the handler analysis would throw
  ;; with an handler that ignores the tag, we can simulate the current behaviour
  ;; of the clojure compiler
  (is (ana.clr/analyze '(defn ^long a [] 1)
                       (ana.clr/empty-env)
                       {:passes-opts {:validate/wrong-tag-handler (fn [t ast]
                                                                    {t nil})}})))
