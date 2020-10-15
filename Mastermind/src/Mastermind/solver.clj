(ns Mastermind.solver
  (:use midje.sweet))

(declare parse-int)
(declare code-secret)
(declare answer)
(declare -main)
(declare proposition)
(declare possible-combinations)
(declare frequences-gcb)
(declare freqs-dispo)
(declare filtre-indications)
(declare indications)
(declare check?)
(declare arret-jeu)
(declare elimi-possi)
(declare elimi-possi2)
(declare vmin)
(declare vmax)
(declare vmax-nth)

(def ^:dynamic *couleurs* '(":rouge" ":bleu" ":vert" ":jaune" ":noir" ":blanc"))
(def ans-poss ({:good 0, :color 0, :bad 4}, {:good 0, :color 1, :bad 3}, {:good 0, :color 2, :bad 2}, {:good 0, :color 3, :bad 1}, {:good 0, :color 4, :bad 0}, {:good 1, :color 0, :bad 3},
  {:good 1, :color 1, :bad 2}, {:good 1, :color 2, :bad 1}, {:good 1, :color 3, :bad 0}, {:good 2, :color 0, :bad 2}, {:good 2, :color 1, :bad 1}, {:good 2, :color 2, :bad 0},
  {:good 3, :color 0, :bad 1}, {:good 3, :color 1, :bad 0}, {:good 4, :color 0, :bad 0}))



(defn parse-int [s]
  (Integer. (re-find  #"\d+" s )))

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

(defn possible-combinations [u]
  (if (= 1 (count u))
    (list u)
    (for [x u, y (possible-combinations (disj (set u) x))]
      (cons x y))))



(defn answer[]
  (print "\nNombre de good : ")
  (flush)
  (let [good (parse-int (read-line))]
    (print "\nNombre de color : ")
    (flush)
    (let [color (parse-int (read-line))]
      (print "\nNombre de bad : ")
      (flush)
      (let [bad (parse-int (read-line))]
        (let [ bla (assoc nil :good good :color color :bad bad)]
          bla)))))

(defn frequences-gcb[u]
  (def rep [:good :color :bad])
  (loop [f rep, u u, res {}]
    (if (seq f)
      (if (contains? u (first f))
        (recur (rest f) u (assoc res (first f) (get u (first f))))
        (recur (rest f) u (assoc res (first f) 0)))
      res)))

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


(defn vmin[s]
  (if (seq s)
    (loop [r (first s),s (rest s)]
      (if (seq s)
        (if (< (compare (first s) r) 0)
          (recur  (first s) (rest s))
          (recur r (rest s)))
        r))
    nil))

(defn vmax[s]
  (if (seq s)
    (loop [r (first s),s (rest s)]
      (if (seq s)
        (if (> (compare (first s) r) 0)
          (recur  (first s) (rest s))
          (recur r (rest s)))
        r))
    nil))

(defn vmax-nth[s]
  (if (seq s)
    (loop [cpt 1, res 0, r (first s),s (rest s)]
      (if (seq s)
        (if (> (compare (first s) r) 0)
          (recur  (+ cpt 1) cpt (first s) (rest s))
          (recur (+ cpt 1) res r (rest s)))
        res))
    nil))


(defn arret-jeu []
  (System/exit 0))

(defn elimi-possi [u v w]
  (loop [s u res []]
    (if (seq s)
      (do
        (let [first (first s)]
          (let [nvfirst (into [] first)]
            (let [nvfirst1 (subvec nvfirst 2)]
              (let [form (check? nvfirst1 v)]
                (let [answer2 (frequences-gcb form)]
                  (if (not (= w answer2))
                    (recur (rest s) (conj res nvfirst1))
                    (recur (rest s) res))))))))
      res)))

(defn elimi-possi2 [u v w]
  (loop [s u, k 0, res []]
    (if (seq s)
      (do
        (let [first (first s)]
          (let [nvfirst (into [] first)]
            (let [nvfirst1 (subvec nvfirst 2)]
              (let [form (check? nvfirst1 v)]
                (let [answer2 (frequences-gcb form)]
                  (if (not (= w answer2))
                    (recur (rest s) k (conj res nvfirst1))
                    (recur (rest s) (inc k) res))))))))
      k)))



(defn -main[]
  (println "Le bot se bande les yeux.")
  (print "Saisissez un code à faire deviner : ")
  (flush)
  (let [my-code (proposition (read-line))]
    (println "Voici votre code : " my-code)
      (let [code [[:rouge, :rouge, :bleu, :bleu]]]
        (loop [k 0, s (first code)]
          (if (= k 0)
            (do
              (print "\nLe bot propose ce code : ")
              (println s)
              (let [answer (answer)]
                (println "\nVous avez saisi : " answer)
                (if (not (= (answer :good) 4))
                  (do
                    (let [pos-combi (possible-combinations [:rouge :bleu :vert :jaune :noir :blanc])]
                      (let [nvlpos-combi (elimi-possi pos-combi s answer)]
                        (loop [s nvlpos-combi, res1{}]
                          (if (seq s)
                            (do
                              (let [new (loop [t ans-poss, res[]]
                                          (if (seq t)
                                            (do
                                              (let [each-poss (elimi-possi2 pos-combi (first s) (first t))]
                                              (recur (rest t) (conj res each-poss))))
                                            res))]
                                (recur (rest s) (assoc res1 (first s) (vmin new)))))
                            (do
                              (let [max (vmax-nth (val res1))]
                                (loop [j 0, res1 res1, max []]
                                  (if (< j max)
                                    (recur (inc j) (rest res1) max)
                                    (recur j res1 (conj max (first (keys res1))))))
                                (conj s (first res1))
                                (recur k (rest s)))))))))
                  (do
                    (println "Vous avez perdu")
                    (arret-jeu))))))))))
