(ns Mastermind.mastermind
  (:use midje.sweet)
  (:use Mastermind.check)
  (:use Mastermind.surroundgame)
  (:use Mastermind.generatetransform))


(declare -main)

(defn -main [args]
  (presentation)
  (loop [m args, j 1, j1 0, j2 0]
    (if (> m 0)
      (do
        (println "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
        (println "\nMANCHE " j "(Rappel des couleurs disponibles : rouge, bleu, vert, jaune, noir, blanc)\n")
        (let [code (code-secret)]
          (def k (main-game code))
          (if (= k 2)
            (do
              (println "\nVous gagnez la " j "e manche\n")
              (recur (- m 1) (inc j) j1 (inc j2)))
            (do
              (println "\nVous perdez la " j "e manche\n")
              (recur (- m 1) (inc j) (inc j1) j2)))))
      (do
        (if (> j1 j2)
          (println "\nLe bot a gagne la partie ...\n")
          (if (< j1 j2)
            (println "\nBRAVO !!!!!!! Vous avez gagne la partie !!!\n")
            (if (= j1 j2)
              (println "\nRetentez votre chance ! Vous y arriverez ...\n"))))
        (arret-jeu)))))

(-main 5)
