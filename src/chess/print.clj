(ns chess.print
  (:require [clojure.string :as string])
  (:require [clojure.term.colors :as colors]))

(def ^:private piece-symbols {:king "♚" :queen "♛" :rook "♜" :bishop "♝" :knight "♞" :pawn "♟︎"})

(def ^:private row-symbols {:1 "1" :2 "2" :3 "3" :4 "4" :5 "5" :6 "6" :7 "7" :8 "8"})

(defn- symbol-for-piece [cell]
  (str
   " "
   (if (nil? cell) " " (-> cell :type piece-symbols))
   " "))

(defn- color-background [nrow ncol]
  (if (nrow #{:1 :3 :5 :7})
    (if (ncol #{:a :c :e :g})
      colors/on-white colors/on-yellow)
    (if (ncol #{:a :c :e :g})
      colors/on-yellow colors/on-white)))

(defn- color-piece [piece]
  (if (= :black (:color piece))
    colors/red
    colors/concealed))

(defn- colors-for-cell [piece nrow ncol]
  [(color-background nrow ncol) (color-piece piece)])

(defn- cell-to-string [piece nrow ncol]
  (reduce
   (fn [symbol color] (color symbol))
   (symbol-for-piece piece)
   (colors-for-cell piece nrow ncol)))

(defn- draw-row [pieces nrow]
  (str " " (name nrow) " "
       (let [cols [:a :b :c :d :e :f :g :h]]
         (string/join "" (map (fn [ncol] (cell-to-string (pieces {:col ncol :row nrow}) nrow ncol)) cols)))))

(defn draw [pieces as]
  (str
   (let [rows [:1 :2 :3 :4 :5 :6 :7 :8]
         rows (if (= as :white) (reverse rows) rows)]
     (string/join "\n" (map (fn [nrow] (draw-row pieces nrow)) rows)))
   "\n    a  b  c  d  e  f  g  h "))
