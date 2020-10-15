(ns Mastermind.generatetransform
  (:use midje.sweet))

(declare code-secret)
(declare proposition)
(declare transform-input)

(def ^:dynamic *couleurs* '(":rouge" ":bleu" ":vert" ":jaune" ":noir" ":blanc"))

(defn proposition [tent]
  (if (nil? (some #(= \, %) (vec tent)))
    (let [color-map (zipmap '(\r \b \v \j \n \w) *couleurs*)]
      (mapv #(color-map %) (vec tent)))
    (mapv #(clojure.string/trim %) (clojure.string/split tent #","))))

(defn code-secret []
  (loop [k 0 , res []]
    (if (< k 4)
      (recur (+ k 1) (conj res (rand-nth [:rouge :bleu :vert :jaune :noir :blanc])))
      res)))

(defn transform-input [tent]
  (loop [ten tent, t [], x 0]
    (if (not= x 4)
      (recur (rest ten) (conj t (name (first ten))) (inc x))
      t)))
