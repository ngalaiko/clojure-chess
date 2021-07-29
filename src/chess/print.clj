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

(defn- bg-for [rank file]
  (if (rank #{:1 :3 :5 :7})
    (if (file #{:a :c :e :g})
      (-> theme :bg-white bg) (-> theme :bg-black bg))
    (if (file #{:a :c :e :g})
      (-> theme :bg-black bg) (-> theme :bg-white bg))))

(defn- fg-for [piece]
  (-> piece :color {:black (-> theme :fg-black fg)
                    :white (-> theme :fg-white fg)
                    nil    str}))

(defn- colors-for [piece rank file]
  [(bg-for rank file) (fg-for piece)])

(defn- draw-piece [square]
  (let [piece-symbols {:king "♚" :queen "♛" :rook "♜" :bishop "♝" :knight "♞" :pawn "♟︎" nil " "}]
    (str " " (-> square :type piece-symbols) " ")))

(defn- draw-square [piece rank file]
  (reduce
   (fn [piece color] (color piece))
   (draw-piece piece)
   (colors-for piece rank file)))

(defn- draw-rank [pieces rank]
  (str " " (name rank) " "
       (let [files [:a :b :c :d :e :f :g :h]]
         (string/join "" (map (fn [file] (draw-square (pieces {:file file :rank rank}) rank file)) files)))))

(defn draw [pieces as]
  (str
   (let [ranks [:1 :2 :3 :4 :5 :6 :7 :8]
         ranks (if (= as :white) (reverse ranks) ranks)]
     (string/join "\n" (map #(draw-rank pieces %) ranks)))
   "\n    a  b  c  d  e  f  g  h "))
