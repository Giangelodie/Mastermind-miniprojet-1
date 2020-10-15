(ns Mastermind.check
  (:use midje.sweet))

(declare check?)
(declare freqs-dispo)
(declare filtre-indications)
(declare indications)


(defn freqs-dispo [u, v]
  (loop [u u, v v, res {}]
    (if (seq u)
      (if (not (= (first v) :good))
        (if (contains? res (first u))
          (recur (rest u) (rest v) (assoc res (first u) (inc (get res (first u)))))
          (recur (rest u) (rest v) (assoc res (first u) 1)))
        (recur (rest u) (rest v ) (assoc res (first u) 0)))
      res)))

(defn filtre-indications [u, v, w]
  (let [my_map (freqs-dispo u w)]
    (loop  [u u, v v, w w, f  my_map, res []]
      (if (seq v)
        (cond
          (= (first w) :color) (if (> (f (first v)) 0)
                                 (recur u (rest v) (rest w) (update f (first v) dec) (conj res (first w)))
                                 (recur u (rest v) (rest w) f (conj res :bad)))
          :else (recur u (rest v) (rest w) f (conj res (first w))))
        res))))

(defn indications [v, u]
  (loop [k 0, s u, t v, res []]
    (if (seq s)
      (if (= (first s) (nth t k))
        (recur (inc k) (rest s) t (conj res :good))
        (if (some #(= (first s) %) t)
            (recur (inc k) (rest s) t (conj res :color))
            (recur (inc k) (rest s) t (conj res :bad))))
      res)))

(defn check? [u, v]
  (let [indic (indications u v)]
    (let [filt (filtre-indications u v indic)]
      (frequencies filt))))
