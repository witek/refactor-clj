(ns refactor-clj.zip
  (:require
   [clojure.test :as t :refer [deftest is]]
   [rewrite-clj.zip :as z]))


(defn of-string [s]
  (z/of-string s))


(defn goto-ns-form
  "Navigate `zipper` to the `ns` form.
  Return `nil` when the `ns` form is missing."
  [zipper]
  (when-let [zipper-ns (z/find-value zipper z/next 'ns)]
    (z/up zipper-ns)))


(deftest goto-ns-form-test
  (is (= 'ns
         (-> "(ns some.core):key" z/of-string goto-ns-form z/next z/sexpr)))
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


(defn with-requires
  "Eval `f` with `value` and each require sexpr from the `ns` form in the
  `zipper`."
  [zipper f value]
  (when-let [zipper (goto-ns-form zipper)]
    (when-let [zipper (z/find-value zipper z/next :require)]
      (when-let [zipper (z/next zipper)]
        (loop [zipper   zipper
               value value]
          (if-not zipper
            value
            (recur (z/right zipper) (f value (z/sexpr zipper)))))))))


(defn with-sexprs
  "Eval `f` with `value` and each sexpr in the `zipper`."
  [zipper f value]
  (loop [zipper zipper
         value value]
    (if (z/end? zipper)
      value
      (recur (z/next zipper) (f value (z/sexpr zipper))))))


(comment
  (def zipper (z/of-string "(ns some.core)(println :hello :owlrd)"))
  (def zipper (z/of-string ":just-a-keyword"))
  (def zipper (z/of-string (str "(ns some.core\n"
                                "  (:require\n"
                                "   [some.utl :as utl :refer [u2]]\n"
                                "   [clojure.edn :as e]))\n"
                                "(defn f1 [] nil)\n"
                                "(defn f2 [f c] (map f c) )\n"
                                "(defn f3 [] (utl/u1))\n"
                                "(defn f4 [] (u2))\n"
                                "(defn f5 [] (some.utl/u3))\n")))
  (def requires {:refers {'u1 'some.utl/u1
                          'u2 'some.utl/u2
                          'u3 'some.utl/u3}
                 :aliases {'utl `some.utl}}))
