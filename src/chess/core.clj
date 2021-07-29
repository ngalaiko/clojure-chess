(ns chess.core
  (:require [chess.game :as game]
            [chess.print :as print])
  (:gen-class))

(def ^:private next-color {:white :black
                           :black :white})

; todo: this is limited by stack size now, remove the limitation
(defn play [color pieces]
  (println)
  (println (print/draw pieces color))
  (print (str (name color) "> "))
  (flush)
  (try (->>
        (read-line)
        (game/move pieces color)
        (play (color next-color)))
       (catch Exception e ((let [error-message (ex-message e)]
                             (println error-message)
                             (play color pieces))))))

(defn -main [& args]
  (println "Welcome to Clojure-chess!")
  (println "Play using algebraic chess notation (i.e. \"e4\" or \"Nc3\")")
  (play :white game/pieces))
