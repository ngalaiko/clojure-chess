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
  (println "")
  (println "Play using algebraic chess notation:")
  (println "R = rook,  N = knight, B = bishop, Q = queen, K = king, pawn is omitted.")
  (println "For example: \"Nc3\" - move knight to c3, \"exd5\" - e pawn takes at d5, \"0-0-0\" - queenside castle")
  (println "Read more: http://whitestone.my-pta.org/Content/1_23/Files/ChessNotation.pdf")
  (play :white game/pieces))
