(ns chess.game)

(defn- piece [color type] {:color color :type type :moved nil})
(defn- square [file rank] {:file file :rank rank})

(def pieces {(square :a :8) (piece :black :rook) (square :b :8) (piece :black :knight) (square :c :8) (piece :black :bishop) (square :d :8) (piece :black :queen) (square :e :8) (piece :black :king) (square :f :8) (piece :black :bishop) (square :g :8) (piece :black :knight) (square :h :8) (piece :black :rook)
             (square :a :7) (piece :black :pawn) (square :b :7) (piece :black :pawn)   (square :c :7) (piece :black :pawn)   (square :d :7) (piece :black :pawn)  (square :e :7) (piece :black :pawn) (square :f :7) (piece :black :pawn)   (square :g :7) (piece :black :pawn)   (square :h :7) (piece :black :pawn)
             (square :a :2) (piece :white :pawn) (square :b :2) (piece :white :pawn)   (square :c :2) (piece :white :pawn)   (square :d :2) (piece :white :pawn)  (square :e :2) (piece :white :pawn) (square :f :2) (piece :white :pawn)   (square :g :2) (piece :white :pawn)   (square :h :2) (piece :white :pawn)
             (square :a :1) (piece :white :rook) (square :b :1) (piece :white :knight) (square :c :1) (piece :white :bishop) (square :d :1) (piece :white :queen) (square :e :1) (piece :white :king) (square :f :1) (piece :white :bishop) (square :g :1) (piece :white :knight) (square :h :1) (piece :white :rook)})

(defn- flip-map [map] (reduce #(assoc %1 (last %2) (first %2)) {} map))

(def ^:private rank-to-int {:1 1 :2 2 :3 3 :4 4 :5 5 :6 6 :7 7 :8 8})
(def ^:private int-to-rank (flip-map rank-to-int))

(def ^:private file-to-int {:a 1 :b 2 :c 3 :d 4 :e 5 :f 6 :g 7 :h 8})
(def ^:private int-to-file (flip-map file-to-int))

(defn- parse-movement [movement]
  (case movement
    "0-0"   {:castle :kingside}
    "0-0-0" {:castle :queenside}
    (let [matcher (re-matcher
                   #"(?<type>[KQBNR])?(?<fromfile>[abcdefgh])?(?<fromrank>[123456789])?(?<captures>x)?(?<file>[abcdefgh])(?<rank>[12345678])=?(?<promotion>[QBNR])?(?<note>[+#!?])?"
                   movement)]
      (if (.matches matcher)
        {:from {:file (keyword (.group matcher "fromfile"))
                :rank (keyword (.group matcher "fromrank"))}
         :type      ({"K" :king "Q" :queen "B" :bishop "N" :knight "R" :rook "P" :pawn}
                     (.group matcher "type")
                     :pawn)
         :captures  (some? (.group matcher "captures"))
         :to    {:file (keyword (.group matcher "file"))
                 :rank (keyword (.group matcher "rank"))}
         :note      ({"+" :check "#" :checkmate "!" :good-move "?" :poor-move}
                     (.group matcher "note"))
         :promotion ({"Q" :queen "B" :bishop "N" :knight "R" :rook}
                     (.group matcher "promotion"))}
        (throw (ex-info (str "Can't parse movement") {:movement movement}))))))

(defn- rank-plus [rank value]
  (let [result (-> rank rank-to-int (+ value) int-to-rank)]
    (if (some? result)
      result
      (throw (ex-info (str "Resulting rank out of bounds") {:rank result})))))

(defn- file-plus [file value]
  (let [result (-> file file-to-int (+ value) int-to-file)]
    (if (some? result)
      result
      (throw (ex-info (str "Resulting fileumn out of bounds") {:file result})))))

(defn- same-rank? [square1 square2]
  (= (:rank square1) (:rank square2)))

(defn- same-file? [square1 square2]
  (= (:file square1) (:file square2)))

(defn- abs [n] (max n (- n)))

(defn- same-diagonal? [square1 square2]
  (let [x1 (-> square1 :rank rank-to-int)
        x2 (-> square2 :rank rank-to-int)
        y1 (-> square1 :file file-to-int)
        y2 (-> square2 :file file-to-int)
        dx (-> x1 (- x2) abs)
        dy (-> y1 (- y2) abs)]
    (= dx dy)))

(defmulti ^:private any-obsticles? (fn [pieces from _] (:type (pieces from))))

(defmethod any-obsticles? :knight [_ _ _] false)

(defmethod any-obsticles? :default [pieces from to]
  (let [rank-from (-> from :rank rank-to-int)
        rank-to   (-> to :rank rank-to-int)
        file-from (-> from :file file-to-int)
        file-to   (-> to :file file-to-int)
        pieces    (assoc pieces from nil)
        [rank-step file-step] (cond
                                (same-rank? from to) (if (< file-from file-to) [0 1] [0 -1])
                                (same-file? from to) (if (< rank-from rank-to) [1 0] [-1 0])
                                (same-diagonal? from to) (cond
                                                           (and (< rank-from rank-to) (< file-from file-to)) [1 1]
                                                           (and (< rank-from rank-to) (> file-from file-to)) [1 -1]
                                                           (and (> rank-from rank-to) (> file-from file-to)) [-1 -1]
                                                           (and (> rank-from rank-to) (< file-from file-to)) [-1 1]))]
    (loop [curr from]
      (cond
        (= curr to) false
        (pieces curr) true
        :else (recur (assoc curr
                            :rank (-> curr :rank (rank-plus rank-step))
                            :file (-> curr :file (file-plus file-step))))))))

(defn- upwards? [square1 square2]
  (< (-> square1 :rank rank-to-int) (-> square2 :rank rank-to-int)))

(defn- downwards? [square1 square2]
  (> (-> square1 :rank rank-to-int) (-> square2 :rank rank-to-int)))

(defn- distance [square1 square2]
  (let [x1 (-> square1 :rank rank-to-int)
        x2 (-> square2 :rank rank-to-int)
        y1 (-> square1 :file file-to-int)
        y2 (-> square2 :file file-to-int)
        dx (-> x1 (- x2) abs)
        dy (-> y1 (- y2) abs)]
    (max dx dy)))

(defmulti ^:private can-move? (fn [pieces from _] (:type (pieces from))))

(defmethod can-move? :rook [pieces from to]
  (and
   (or
    (same-file? from to)
    (same-rank? from to))
   (not (any-obsticles? pieces from to))))

(defmethod can-move? :knight [_ from to]
  (let [from-x (-> from :rank rank-to-int)
        from-y (-> from :file file-to-int)
        to-x   (-> to   :rank rank-to-int)
        to-y   (-> to   :file file-to-int)]
    (or
     (and (-> from-x (+ 2) (= to-x)) (-> from-y (+ 1) (= to-y)))
     (and (-> from-x (+ 2) (= to-x)) (-> from-y (- 1) (= to-y)))
     (and (-> from-x (- 2) (= to-x)) (-> from-y (+ 1) (= to-y)))
     (and (-> from-x (- 2) (= to-x)) (-> from-y (- 1) (= to-y)))
     (and (-> from-x (+ 1) (= to-x)) (-> from-y (+ 2) (= to-y)))
     (and (-> from-x (+ 1) (= to-x)) (-> from-y (- 2) (= to-y)))
     (and (-> from-x (- 1) (= to-x)) (-> from-y (+ 2) (= to-y)))
     (and (-> from-x (- 1) (= to-x)) (-> from-y (- 2) (= to-y))))))

(defmethod can-move? :bishop [pieces from to]
  (and
   (same-diagonal? from to)
   (not (any-obsticles? pieces from to))))

(defmethod can-move? :queen [pieces from to]
  (and
   (or
    (same-file? from to)
    (same-rank? from to)
    (same-diagonal? from to))
   (not (any-obsticles? pieces from to))))

(defmethod can-move? :king [pieces from to]
  (and
   (<= (distance from to) 1)
   (or
    (same-file? from to)
    (same-rank? from to)
    (same-diagonal? from to))
   (not (any-obsticles? pieces from to))))

(defmethod can-move? :pawn [pieces from to]
  (and
   (same-file? from to)
   (case (:color (pieces from))
     :white (upwards? from to)
     :black (downwards? from to))
   (case (:rank from)
     :2 (<= (distance from to) 2)
     :7 (<= (distance from to) 2)
     (= 1 (distance from to)))
   (not (any-obsticles? pieces from to))))

(defn- find-last-move [pieces]
  (->> pieces
       (map #(-> % last :moved))
       (map #(if (nil? %) 0 %))
       (apply max)))

(defn- en-passant? [pieces from at]
  (let [target (pieces at)]
    (and
     (nil? target)
     (let [last-move (find-last-move pieces)
           piece (pieces from)
           en-passant-target (pieces (square (:file at) (:rank from)))]
       (and
        (= :pawn (:type en-passant-target))
        (not= (:color en-passant-target) (:color piece))
        (= last-move (:moved en-passant-target)))))))

(defmulti ^:private can-capture? (fn [pieces from _] (:type (pieces from))))

(defmethod can-capture? :pawn [pieces from at]
  (let [target (pieces at)]
    (and
     (or
      target
      (en-passant? pieces from at))
     (not=
      (:color (pieces from))
      (:color target))
     (same-diagonal? from at)
     (= 1 (distance from at))
     (case (:color (pieces from))
       :white (upwards? from at)
       :black (downwards? from at)))))

(defmethod can-capture? :default [pieces from at]
  (let [target (pieces at)]
    (and
     target
     (not=
      (:color (pieces from))
      (:color target))
     (can-move? pieces from at))))

(defn- eligible-for-promotion? [piece to]
  (and
   (= (:type piece) :pawn)
   (= (:rank to) (case (:color piece)
                   :white :8
                   :black :1))))

(defn- find-a-piece-to-move [pieces color movement]
  (let [{type :type
         to   :to
         captures? :captures
         {from-file :file
          from-rank :rank} :from} movement
        promotion (:promotion movement)]
    (first
     (filter
      #(let [from (first %)
             piece (last %)]
         (and
          (-> piece :type  (= type))
          (-> piece :color (= color))
          (if from-file (= from-file (:file from)) true)
          (if from-rank (= from-rank (:rank from)) true)
          (if promotion (eligible-for-promotion? piece to) true)
          (if captures?
            (can-capture? pieces from to)
            (can-move? pieces from to))))
      pieces))))

(defn- get-moves [pieces color movement]
  (let [promote-to (:promotion movement)
        move-to (:to movement)
        [move-from piece] (find-a-piece-to-move pieces color movement)]
    (when (nil? piece)
      (throw (ex-info (str "Can't move like that") {:move movement})))
    (when (and
           (eligible-for-promotion? piece move-to)
           (nil? promote-to))
      (throw (ex-info (str "Pawn must be promoted to either Knight, Bishop, Rook or Queen") {})))
    [(fn [pieces] ;; move piece
       (let [piece (pieces move-from)]
         (assoc pieces
                move-from nil
                move-to piece)))
     (fn [pieces] ;; mark piece as moved
       (let [piece (pieces move-to)
             last-move (find-last-move pieces)
             moved-piece (assoc piece :moved (+ 1 last-move))]
         (assoc pieces
                move-to moved-piece)))
     (if promote-to ;; promote piece
       (fn [pieces]
         (let [piece (pieces move-to)
               promoted-piece (assoc piece :type promote-to)]
           (assoc pieces
                  move-to promoted-piece)))
       (fn [pieces] pieces) ;; noop
       )
     (if (en-passant? pieces move-from move-to)
       (fn [pieces] ;; en passant capture
         (let [en-passant-target (square (:file move-to) (:rank move-from))]
           (assoc pieces
                  en-passant-target nil)))
       (fn [pieces] pieces) ;; noop
       )]))

(defn- castle [pieces {{king-from :from king-to :to} :king
                       {rook-from :from rook-to :to} :rook}]
  (let [king (pieces king-from)
        rook (pieces rook-from)
        last-move (find-last-move pieces)]
    (when (:moved king)
      (throw (ex-info (str "Can't castle: king was moved") {})))
    (when (:moved rook)
      (throw (ex-info (str "Can't castle: rook was moved") {})))
    (when (any-obsticles? pieces king-from rook-from)
      (throw (ex-info (str "Can't castle: something is in the way") {})))
    (assoc pieces
           king-from nil
           rook-from nil
           king-to (assoc king :moved (+ 1 last-move))
           rook-to (assoc rook :moved (+ 1 last-move)))))

(defn- castle-queenside [pieces color]
  (let [rank (color {:white :1
                     :black :8})]
    (castle pieces {:king {:from (square :e rank)
                           :to   (square :c rank)}
                    :rook {:from (square :a rank)
                           :to   (square :d rank)}})))

(defn- castle-kingside [pieces color]
  (let [rank (color {:white :1
                     :black :8})]
    (castle pieces {:king {:from (square :e rank)
                           :to   (square :g rank)}
                    :rook {:from (square :h rank)
                           :to   (square :f rank)}})))

(defn move [pieces color move]
  (let [movement (parse-movement move)]
    (case (:castle movement)
      :kingside  (castle-kingside  pieces color)
      :queenside (castle-queenside pieces color)
      (reduce
       (fn [pieces move] (move pieces))
       pieces
       (get-moves pieces color movement)))))
