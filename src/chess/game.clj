(ns chess.game)

(defn- piece [color type] {:color color :type type :moved false})
(defn- cell [col row] {:col col :row row})

(def pieces {(cell :a :8) (piece :black :rook) (cell :b :8) (piece :black :knight) (cell :c :8) (piece :black :bishop) (cell :d :8) (piece :black :queen) (cell :e :8) (piece :black :king) (cell :f :8) (piece :black :bishop) (cell :g :8) (piece :black :knight) (cell :h :8) (piece :black :rook)
             (cell :a :7) (piece :black :pawn) (cell :b :7) (piece :black :pawn)   (cell :c :7) (piece :black :pawn)   (cell :d :7) (piece :black :pawn)  (cell :e :7) (piece :black :pawn) (cell :f :7) (piece :black :pawn)   (cell :g :7) (piece :black :pawn)   (cell :h :7) (piece :black :pawn)
             (cell :a :2) (piece :white :pawn) (cell :b :2) (piece :white :pawn)   (cell :c :2) (piece :white :pawn)   (cell :d :2) (piece :white :pawn)  (cell :e :2) (piece :white :pawn) (cell :f :2) (piece :white :pawn)   (cell :g :2) (piece :white :pawn)   (cell :h :2) (piece :white :pawn)
             (cell :a :1) (piece :white :rook) (cell :b :1) (piece :white :knight) (cell :c :1) (piece :white :bishop) (cell :d :1) (piece :white :queen) (cell :e :1) (piece :white :king) (cell :f :1) (piece :white :bishop) (cell :g :1) (piece :white :knight) (cell :h :1) (piece :white :rook)})

(defn- flip-map [map] (reduce #(assoc %1 (last %2) (first %2)) {} map))

(def ^:private row-to-int {:1 1 :2 2 :3 3 :4 4 :5 5 :6 6 :7 7 :8 8})
(def ^:private int-to-row (flip-map row-to-int))

(def ^:private col-to-int {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8})
(def ^:private int-to-col (flip-map col-to-int))

(defn- parse-movement [movement]
  (case movement
    "0-0"   {:note :kingside-castle}
    "0-0-0" {:note :queenside-castle}
    (let [matcher (re-matcher
                   #"(?<type>[KQBNR])?(?<hint>[abcdefgh])?(?<captures>x)?(?<col>[abcdefgh])(?<row>[12345678])(?<note>[+#!?])?"
                   movement)]
      (if (.matches matcher)
        {:hint     (keyword (.group matcher "hint"))
         :type     ({"K" :king "Q" :queen "B" :bishop "N" :knight "R" :rook "P" :pawn}
                    (.group matcher "type")
                    :pawn)
         :captures (some? (.group matcher "captures"))
         :cell     (cell (keyword (.group matcher "col")) (keyword (.group matcher "row")))
         :note     ({"+" :check "#" :checkmate "!" :good-move "?" :poor-move}
                    (.group matcher "note"))}
        (throw (ex-info (str "Can't parse movement") {:movement movement}))))))

(defn- row-plus [row value]
  (let [result (-> row row-to-int (+ value) int-to-row)]
    (if (some? result)
      result
      (throw (ex-info (str "Resulting row out of bounds") {:row result})))))

(defn- col-plus [col value]
  (let [result (-> col col-to-int (+ value) int-to-col)]
    (if (some? result)
      result
      (throw (ex-info (str "Resulting column out of bounds") {:col result})))))

(defn- same-row? [cell1 cell2]
  (= (:row cell1) (:row cell2)))

(defn- same-col? [cell1 cell2]
  (= (:col cell1) (:col cell2)))

(defn- abs [n] (max n (- n)))

(defn- same-diagonal? [cell1 cell2]
  (let [x1 (-> cell1 :row row-to-int)
        x2 (-> cell2 :row row-to-int)
        y1 (-> cell1 :col col-to-int)
        y2 (-> cell2 :col col-to-int)
        dx (-> x1 (- x2) abs)
        dy (-> y1 (- y2) abs)]
    (= dx dy)))

(defn- upwards? [cell1 cell2]
  (< (-> cell1 :row row-to-int) (-> cell2 :row row-to-int)))

(defn- downwards? [cell1 cell2]
  (> (-> cell1 :row row-to-int) (-> cell2 :row row-to-int)))

(defn- distance [cell1 cell2]
  (let [x1 (-> cell1 :row row-to-int)
        x2 (-> cell2 :row row-to-int)
        y1 (-> cell1 :col col-to-int)
        y2 (-> cell2 :col col-to-int)
        dx (-> x1 (- x2) abs)
        dy (-> y1 (- y2) abs)]
    (max dx dy)))

(defmulti ^:private can-move? (fn [pieces from _] (:type (pieces from))))

(defmethod can-move? :rook [_ from to]
  (or
   (same-col? from to)
   (same-row?  from to)))

(defmethod can-move? :knight [_ from to]
  (let [from-x (-> from :row row-to-int)
        from-y (-> from :col col-to-int)
        to-x   (-> to   :row row-to-int)
        to-y   (-> to   :col col-to-int)]
    (or
     (and (-> from-x (+ 2) (= to-x)) (-> from-y (+ 1) (= to-y)))
     (and (-> from-x (+ 2) (= to-x)) (-> from-y (- 1) (= to-y)))
     (and (-> from-x (- 2) (= to-x)) (-> from-y (+ 1) (= to-y)))
     (and (-> from-x (- 2) (= to-x)) (-> from-y (- 1) (= to-y)))
     (and (-> from-x (+ 1) (= to-x)) (-> from-y (+ 2) (= to-y)))
     (and (-> from-x (+ 1) (= to-x)) (-> from-y (- 2) (= to-y)))
     (and (-> from-x (- 1) (= to-x)) (-> from-y (+ 2) (= to-y)))
     (and (-> from-x (- 1) (= to-x)) (-> from-y (- 2) (= to-y))))))

(defmethod can-move? :bishop [_ from to]
  (same-diagonal? from to))

(defmethod can-move? :queen [_ from to]
  (or
   (same-col? from to)
   (same-row?  from to)
   (same-diagonal?  from to)))

(defmethod can-move? :king [_ from to]
  (and
   (<= (distance from to) 1)
   (or
    (same-col? from to)
    (same-row?  from to)
    (same-diagonal?  from to))))

(defmethod can-move? :pawn [pieces from to]
  (and
   (same-col? from to)
   (case (:color (pieces from))
     :white (upwards? from to)
     :black (downwards? from to))
   (case (:row from)
     :2 (<= (distance from to) 2)
     :7 (<= (distance from to) 2)
     (= 1 (distance from to)))))

(defmulti ^:private can-capture? (fn [pieces from _] (:type (pieces from))))

(defmethod can-capture? :pawn [pieces from at]
  (and
   (same-diagonal? from at)
   (= 1 (distance from at))
   (case (:color (pieces from))
     :white (upwards? from at)
     :black (downwards? from at))))

(defmethod can-capture? :default [pieces from at]
  (can-move? pieces from at))

(defmulti ^:private any-obsticles? (fn [pieces from _] (:type (pieces from))))

(defmethod any-obsticles? :knight [_ _ _] false)

(defmethod any-obsticles? :default [pieces from to]
  (let [row-from (-> from :row row-to-int)
        row-to   (-> to :row row-to-int)
        col-from (-> from :col col-to-int)
        col-to   (-> to :col col-to-int)
        pieces   (assoc pieces from nil to nil)
        step     (cond
                   (same-row? from to) (if (< col-from col-to) [0 1] [0 -1])
                   (same-col? from to) (if (< row-from row-to) [1 0] [-1 0])
                   (same-diagonal? from to) (cond
                                              (and (< row-from row-to) (< col-from col-to)) [1 1]
                                              (and (< row-from row-to) (> col-from col-to)) [1 -1]
                                              (and (> row-from row-to) (> col-from col-to)) [-1 -1]
                                              (and (> row-from row-to) (< col-from col-to)) [-1 1]))]
    (loop [curr from]
      (cond
        (= curr to) false
        (pieces curr) true
        :else (recur (assoc curr
                            :row (-> curr :row (row-plus (first step)))
                            :col (-> curr :col (col-plus (last step)))))))))

(defn- find-piece [pieces color movement]
  (let [type (:type movement)
        to   (:cell movement)
        hint (:hint movement)
        target (pieces to)]
    (first
     (filter
      #(let [piece (last %) from (first %)]
         (and
          (-> piece :type  (= type))
          (-> piece :color (= color))
          (if hint (= hint (:col from)) true)
          (if target
            (and
             (not= color (:color target))
             (can-capture? pieces from to))
            (can-move? pieces from to))
          (not (any-obsticles? pieces from to))))
      pieces))))

(defn- castle [pieces {{king-from :from king-to :to} :king
                       {rook-from :from rook-to :to} :rook}]
  (let [king (pieces king-from)
        rook (pieces rook-from)]
    (when (:moved king) (throw (ex-info (str "Can't castle: king was moved") {})))
    (when (:moved rook) (throw (ex-info (str "Can't castle: rook was moved") {})))
    (when (any-obsticles? pieces king-from rook-from) (throw (ex-info (str "Can't castle: something is in the way") {})))
    (assoc pieces
           king-from nil
           rook-from nil
           king-to (assoc king :moved true)
           rook-to (assoc rook :moved true))))

(defn- castle-kingside [pieces color]
  (let [row (color {:white :1
                    :black :8})]
    (castle pieces {:king {:from (cell :e row)
                           :to   (cell :g row)}
                    :rook {:from (cell :h row)
                           :to   (cell :f row)}})))

(defn- castle-queenside [pieces color]
  (let [row (color {:white :1
                    :black :8})]
    (castle pieces {:king {:from (cell :e row)
                           :to   (cell :c row)}
                    :rook {:from (cell :a row)
                           :to   (cell :d row)}})))

(defn move [pieces color movement]
  (let [movement (parse-movement movement)]
    (case (:note movement)
      :kingside-castle  (castle-kingside  pieces color)
      :queenside-castle (castle-queenside pieces color)
      (let [to (:cell movement)
            [from piece] (find-piece pieces color movement)]
        (when (nil? piece) (throw (ex-info (str "Invalid move") {:move movement})))
        (assoc pieces
               from nil
               to (assoc piece :moved true))))))
