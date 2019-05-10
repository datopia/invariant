(ns invariant.unparse
  (:require [datahike.parser :as parser])
  (:import  [datahike.parser
             PlainSymbol Constant SrcVar Aggregate Query FindScalar FindRel
             Pattern Variable BindScalar Predicate Function BindColl BindTuple]))

;; inverse to p/parse-query
(defmulti unparse type)

(defmethod unparse Function
  [f]
  [(concat [(unparse (:fn f))]
           (map unparse (:args f)))
   (unparse (:binding f))])

(defmethod unparse PlainSymbol
  [s]
  (:symbol s))

(defmethod unparse Constant
  [c]
  (:value c))

(defmethod unparse SrcVar
  [s]
  (:symbol s))

(defmethod unparse Query
  [{:keys [qfind qwith qin qwhere]}]
  (vec
   (concat
    [:find] (unparse qfind)
    (when-not (empty? qwith)
      (conj (map unparse qwith) :with))
    (conj (map unparse qin) :in)
    [:where]
    (mapv (comp vec unparse) qwhere))))

(defmethod unparse FindScalar
  [s]
  [(unparse (:element s)) '.])

(defmethod unparse Variable
  [v]
  (:symbol v))

(defmethod unparse BindScalar
  [v]
  (unparse (:variable v)))

(defmethod unparse BindColl
  [bc]
  [(unparse (:binding bc)) '...])

(defmethod unparse BindTuple
  [bt]
  (mapv unparse (:bindings bt)))

(defmethod unparse Predicate
  [{:keys [fn args] :as p}]
  [(conj (map unparse args)
         (unparse fn))])

(defmethod unparse FindRel
  [fr]
  (map unparse (:elements fr)))

(defmethod unparse Aggregate
  [{:keys [fn args]}]
  (conj (map unparse args)
        (unparse fn)))

(defmethod unparse Pattern
  [{:keys [source pattern] :as p}]
  (concat [(unparse source)] (map unparse pattern)))
