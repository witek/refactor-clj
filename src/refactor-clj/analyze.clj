(ns refactor-clj.analyze
  (:require
   [clojure.test :as t :refer [deftest is]]
   [refactor-clj.zip :as zip]))


(defn extract-requires
  [zipper]
  (zip/with-requires
    zipper
    (fn [requires sexpr]
      (let [[ns & {:keys [as refer]}] sexpr
            requires                  (if as
                                        (assoc-in requires [:aliases as] ns)
                                        requires)
            requires                  (reduce
                                       (fn [requires r]
                                         (assoc-in requires [:refers r]
                                                   (symbol (str ns) (str r))))
                                       requires
                                       refer)]
        requires))
    {:aliases {} :refers  {}}))


(defn qualify-ref
  "If `zipper` is located at a reference, return it's qualified symbol."
  [sexpr
   requires]
  (let [{:keys [refers aliases]} requires]
    (cond
      (qualified-symbol? sexpr)
      (let [ns (namespace sexpr)
            alias (get aliases (symbol ns))]
        (if alias
          (symbol (str alias) (-> sexpr name str))
          sexpr))

      (and (symbol? sexpr) (contains? refers sexpr))
      (get refers sexpr)

      :else nil)))


(defn extract-refs
  "Return a list of all references/symbols."
  [zipper requires]
  (zip/with-sexprs
   zipper
   (fn [refs sexpr]
     (if-let [ref (qualify-ref sexpr requires)]
       (conj refs ref)
       refs))
   #{}))


(defn analyze-zipper [zipper]
  (let [requires (extract-requires zipper)
        refs (extract-refs zipper requires)]
    {:zipper zipper
     :requires requires
     :refs refs}))


(deftest analyze-zipper-test
  (let [zipper (zip/of-string (str "(ns some.core\n"
                                   "  (:require\n"
                                   "   [some.utl :as utl :refer [u2]]))\n"
                                   "(defn f1 [] nil)\n"
                                   "(defn f2 [f c] (map f c) )\n"
                                   "(defn f3 [] (utl/u1))\n"
                                   "(defn f4 [] (u2))\n"
                                   "(defn f5 [] (some.utl/u3))\n"))]
    (is (= {:refers {'u2 'some.utl/u2}
            :aliases {'utl 'some.utl}}
           (-> zipper analyze-zipper :requires)))
    (is (= #{'some.utl/u1 'some.utl/u2 'some.utl/u3}
           (-> zipper analyze-zipper :refs)))))


(comment
  (def zipper (zip/of-string (str "(ns some.core\n"
                                  "  (:require\n"
                                  "   [some.utl :as utl :refer [u2]]\n"
                                  "   [clojure.edn :as e]))\n"
                                  "(defn f1 [] nil)\n"
                                  "(defn f2 [f c] (map f c) )\n"
                                  "(defn f3 [] (utl/u1))\n"
                                  "(defn f4 [] (u2))\n"
                                  "(defn f5 [] (some.utl/u3))\n")))
  (-> zipper analyze-zipper :refs))
