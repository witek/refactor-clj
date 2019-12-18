(ns refactor-clj.zipping
  (:require
   [clojure.test :as t :refer [deftest is]]
   [rewrite-clj.parser :as p]
   [rewrite-clj.zip :as z]
   [rewrite-clj.node :as n]))


(defn goto-ns-form
  "Navigate `zipper` to the `ns` form.
  Return `nil` when the `ns` form is missing."
  [zipper]
  (when-let [zipper-ns (z/find-value zipper z/next 'ns)]
    (z/up zipper-ns)))


(deftest goto-ns-form-test
  (is (= 'ns
         (-> "(ns some.core):key" z/of-string goto-ns-form z/next z/node n/sexpr)))
  (is (= nil
         (-> ":key" z/of-string goto-ns-form))))


(defn extract-ns
  "Return the namespace symbol."
  [zipper]
  (when-let [ns-form (goto-ns-form zipper)]
    (-> ns-form z/next z/next z/sexpr)))


(deftest extract-ns-test
  (is (= 'some.core
         (-> "(ns some.core):key" z/of-string extract-ns)))
  (is (= nil
         (-> ":key" z/of-string extract-ns))))


(defn qualify-ref
  "If `zipper` is located at a reference, return it's qualified symbol."
  [zipper
   requires]
  (let [{:keys [refers aliases]} requires
        sexpr (z/sexpr zipper)]
    (cond
      (qualified-symbol? sexpr)
      (let [ns (namespace sexpr)
            alias (get aliases (symbol ns))]
        (if alias
          (symbol (str alias) (-> sexpr name str))
          sexpr))

      (and (symbol? sexpr) (contains? refers sexpr))
      (symbol (-> refers (get sexpr) str)
              (str sexpr))

      :else nil)))


(defn extract-refs
  "Return a list of all references/symbols."
  [zipper requires]
  (loop [zipper zipper
         refs  #{}]
    (if (z/end? zipper)
      refs
      (if-let [ref (qualify-ref zipper requires)]
        (recur (z/next zipper) (conj refs ref))
        (recur (z/next zipper) refs)))))


(deftest extract-refs-test
  (let [zipper (z/of-string (str "(ns some.core\n"
                                 "  (:require\n"
                                 "   [some.utl :as utl :refer [u2]]))\n"
                                 "(defn f1 [] nil)\n"
                                 "(defn f2 [f c] (map f c) )\n"
                                 "(defn f3 [] (utl/u1))\n"
                                 "(defn f4 [] (u2))\n"
                                 "(defn f5 [] (some.utl/u3))\n"))
        requires {:refers  {'u1 'some.utl
                            'u2 'some.utl
                            'u3 'some.utl}
                  :aliases {'utl `some.utl}}]
    (is (= #{'some.utl/u1 'some.utl/u2 'some.utl/u3}
           (extract-refs zipper requires)))))


(comment
  (def zipper (z/of-string "(ns some.core)(println :hello :owlrd)"))
  (def zipper (z/of-string ":just-a-keyword"))
  (def zipper (z/of-string (str "(ns some.core\n"
                                "  (:require\n"
                                "   [some.utl :as utl :refer [u2]]))\n"
                                "(defn f1 [] nil)\n"
                                "(defn f2 [f c] (map f c) )\n"
                                "(defn f3 [] (utl/u1))\n"
                                "(defn f4 [] (u2))\n"
                                "(defn f5 [] (some.utl/u3))\n")))
  (def requires {:refers {'u1 'some.utl
                          'u2 'some.utl
                          'u3 'some.utl}
                 :aliases {'utl `some.utl}}))
