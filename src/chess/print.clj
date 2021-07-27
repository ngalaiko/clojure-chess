(ns chess.print
  (:require [clojure.string :as string]))

; https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit
(def ^:private theme {:bg-black "14"
                      :bg-white "6"
                      :fg-black "0"
                      :fg-white "15"})

(def ^:private ESC "\033")

(defn- set-fg [code]
  (str ESC "[38;5;" code "m"))

(defn- set-bg [code]
  (str ESC "[48;5;" code "m"))

(defn- reset-fg []
  (str ESC "[m"))

(defn- reset-bg []
  (str ESC "[m"))

(defn- fg [code]
  (fn [string]
    (str (set-fg code) string (reset-fg))))

(defn- bg [code]
  (fn [string]
    (str (set-bg code) string (reset-bg))))

(def ^:private piece-symbols {:king "♚" :queen "♛" :rook "♜" :bishop "♝" :knight "♞" :pawn "♟︎"})

(defn- color-cell [nrow ncol]
  (if (nrow #{:1 :3 :5 :7})
    (if (ncol #{:a :c :e :g})
      (-> theme :bg-white bg) (-> theme :bg-black bg))
    (if (ncol #{:a :c :e :g})
      (-> theme :bg-black bg) (-> theme :bg-white bg))))

(defn- color-piece [piece]
  (if (= :black (:color piece))
    (-> theme :fg-black fg)
    (-> theme :fg-white fg)))

(defn- color [piece nrow ncol]
  [(color-cell nrow ncol)
   (color-piece piece)])

(defn- draw-piece [cell]
  (str
   " "
   (if (nil? cell) " " (-> cell :type piece-symbols))
   " "))

(defn- cell-to-string [piece nrow ncol]
  (reduce
   (fn [symbol color] (color symbol))
   (draw-piece piece)
   (color piece nrow ncol)))

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
