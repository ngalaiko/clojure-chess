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

(defn- reset []
  (str ESC "[m"))

(defn- fg [code]
  (fn [string]
    (str (set-fg code) string (reset))))

(defn- bg [code]
  (fn [string]
    (str (set-bg code) string (reset))))

(defn- bg-for [nrow ncol]
  (if (nrow #{:1 :3 :5 :7})
    (if (ncol #{:a :c :e :g})
      (-> theme :bg-white bg) (-> theme :bg-black bg))
    (if (ncol #{:a :c :e :g})
      (-> theme :bg-black bg) (-> theme :bg-white bg))))

(defn- fg-for [piece]
  (-> piece :color {:black (-> theme :fg-black fg)
                    :white (-> theme :fg-white fg)
                    nil    str}))

(defn- colors-for [piece nrow ncol]
  [(bg-for nrow ncol) (fg-for piece)])

(defn- draw-piece [cell]
  (let [piece-symbols {:king "♚" :queen "♛" :rook "♜" :bishop "♝" :knight "♞" :pawn "♟︎" nil " "}]
    (str " " (-> cell :type piece-symbols) " ")))

(defn- draw-cell [piece nrow ncol]
  (reduce
   (fn [piece color] (color piece))
   (draw-piece piece)
   (colors-for piece nrow ncol)))

(defn- draw-row [pieces nrow]
  (str " " (name nrow) " "
       (let [cols [:a :b :c :d :e :f :g :h]]
         (string/join "" (map (fn [ncol] (draw-cell (pieces {:col ncol :row nrow}) nrow ncol)) cols)))))

(defn draw [pieces as]
  (str
   (let [rows [:1 :2 :3 :4 :5 :6 :7 :8]
         rows (if (= as :white) (reverse rows) rows)]
     (string/join "\n" (map #(draw-row pieces %) rows)))
   "\n    a  b  c  d  e  f  g  h "))
