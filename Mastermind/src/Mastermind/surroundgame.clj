(ns Mastermind.surroundgame
  (:use midje.sweet)
  (:use Mastermind.generatetransform)
  (:use Mastermind.check))


(declare main-game)
(declare presentation)
(declare arret-jeu)

(defn presentation []
  (println "\n-------------------------------------------------------------------------------------------\n")
  (println "                                BIENVENUE SUR LE JEU MASTERMIND")
  (println "\n-------------------------------------------------------------------------------------------\n")
  (println "Le but du jeu est de trouver la bonne combinaison de 4 couleurs en moins de 12 tentatives.")
  (println "\nVoici la liste des couleurs possibles : rouge, bleu, vert, jaune, noir, blanc.")
  (println "\nSi vous obtenez :")
  (println "♦ :good : Bonne couleur, bonne position")
  (println "♦ :color : Bonne couleur, mauvaise position")
  (println "♦ :bad : Pas de pion de cette couleur dans la combinaison")
  (println "\n                                     BONNE CHANCE !\n")
  (println "==========================================================================================\n"))



(defn main-game [code]
  (println "\nVous avez 12 tentatives : (exemple de tentative : rouge, vert, jaune, bleu)\n")
  (loop [k 0]
    (if (< k 12)
      (do
        (print "\n♦ Votre" (+ 1 k) "e tentative : ")
        (flush)
        (let [tent (proposition (read-line))]
          (let [t (transform-input code)]
            (println "\nVous avez saisi : " tent)
            (let [check (check? t tent)]
              (println "\nResultat : "check)
              (println "\n---------------------")
              (if (not (= (check :good) 4))
                (recur (inc k))
                2)))))
      1)))

(defn arret-jeu []
  (System/exit 0))
