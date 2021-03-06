(ns chess.game-test
  (:require  [clojure.test :refer :all]
             [chess.game :refer :all]))

(deftest invalid-moves
  (testing "King captures protected piece"
    (is (thrown-with-msg? Exception #"Can't capture with king at g3"
                          (-> {{:file :g :rank :8} {:color :white :type :rook}
                               {:file :g :rank :3} {:color :white :type :pawn}
                               {:file :h :rank :2} {:color :black :type :king}}
                              (move :black "Kxg3")))))

  (testing "King captures own queen"
    (is (thrown-with-msg? Exception #"Can't capture with king at g3"
                          (-> {{:file :g :rank :3} {:color :black :type :pawn}
                               {:file :h :rank :2} {:color :black :type :king}}
                              (move :black "Kxg3")))))

  (testing "Knight captures nothing"
    (is (thrown-with-msg? Exception #"Can't capture with knight at e2"
                          (-> {{:file :g :rank :3} {:color :black :type :knight}}
                              (move :black "Nxe2")))))

  (testing "Pawn moves to occupied piece"
    (is (thrown-with-msg? Exception #"Can't move pawn to e5"
                          (-> {{:file :e :rank :5} {:color :white :type :pawn}
                               {:file :e :rank :4} {:color :white :type :pawn}}
                              (move :white "e5")))))

  (testing "Qeen jumps over own piece"
    (is (thrown-with-msg? Exception #"Can't move queen to e2"
                          (-> {{:file :e :rank :8} {:color :white :type :queen}
                               {:file :e :rank :4} {:color :white :type :rook}}
                              (move :white "Qe2")))))

  (testing "Bishop jumps over opponents piece"
    (is (thrown-with-msg? Exception #"Can't move bishop to b7"
                          (-> {{:file :f :rank :3} {:color :black :type :bishop}
                               {:file :d :rank :5} {:color :white :type :pawn}}
                              (move :black "Bb7")))))

  (testing "King moves under check"
    (is (thrown-with-msg? Exception #"Can't move king to d1"
                          (-> {{:file :e :rank :1} {:color :white :type :king}
                               {:file :d :rank :8} {:color :black :type :rook}}
                              (move :white "Kd1")))))

  (testing "Pawn reaches the last rank without promotion"
    (is (thrown-with-msg? Exception #"Pawn must be promoted to either Knight, Bishop, Rook or Queen"
                          (-> {{:file :e :rank :7} {:color :white :type :pawn}}
                              (move :white "e8")))))

  (testing "Castling after the king was moved"
    (is (thrown-with-msg? Exception #"Can't castle: king was moved"
                          (-> {{:file :a :rank :8} {:color :black :type :rook}
                               {:file :e :rank :8} {:color :black :type :king :moved 3}}
                              (move :black "0-0-0")))))

  (testing "Castling after the rook was moved"
    (is (thrown-with-msg? Exception #"Can't castle: rook was moved"
                          (-> {{:file :h :rank :1} {:color :black :type :rook :moved 1}
                               {:file :e :rank :1} {:color :black :type :king}}
                              (move :white "0-0")))))

  (testing "En passant on the next move"
    (is (thrown-with-msg? Exception #"Can't capture with e pawn at d6"
                          (-> {{:file :d :rank :5} {:color :black :type :pawn :moved 1}
                               {:file :e :rank :5} {:color :white :type :pawn :moved 2}}
                              (move :white "exd6"))))))

(deftest valid-games
  (testing "Kasparov vs. Topalov, Wijk aan Zee 1999"
    (-> pieces
        (move :white "e4")    (move :black "d6")
        (move :white "d4")    (move :black "Nf6")
        (move :white "Nc3")   (move :black "g6")
        (move :white "Be3")   (move :black "Bg7")
        (move :white "Qd2")   (move :black "c6")
        (move :white "f3")    (move :black "b5")
        (move :white "Nge2")  (move :black "Nbd7")
        (move :white "Bh6")   (move :black "Bxh6")
        (move :white "Qxh6")  (move :black "Bb7")
        (move :white "a3")    (move :black "e5")
        (move :white "0-0-0") (move :black "Qe7")
        (move :white "Kb1")   (move :black "a6")
        (move :white "Nc1")   (move :black "0-0-0")
        (move :white "Nb3")   (move :black "exd4")
        (move :white "Rxd4")  (move :black "c5")
        (move :white "Rd1")   (move :black "Nb6")
        (move :white "g3")    (move :black "Kb8")
        (move :white "Na5")   (move :black "Ba8")
        (move :white "Bh3")   (move :black "d5")
        (move :white "Qf4+")  (move :black "Ka7")
        (move :white "Rhe1")  (move :black "d4")
        (move :white "Nd5")   (move :black "Nbxd5")
        (move :white "exd5")  (move :black "Qd6")
        (move :white "Rxd4")  (move :black "cxd4")
        (move :white "Re7+")  (move :black "Kb6")
        (move :white "Qxd4+") (move :black "Kxa5")
        (move :white "b4+")))

  (testing "Morphy vs. Allies, Paris Opera 1858"
    (-> pieces
        (move :white "e4")    (move :black "e5")
        (move :white "Nf3")   (move :black "d6")
        (move :white "d4")    (move :black "Bg4")
        (move :white "dxe5")  (move :black "Bxf3")
        (move :white "Qxf3")  (move :black "dxe5")
        (move :white "Bc4")   (move :black "Nf6")
        (move :white "Qb3")   (move :black "Qe7")
        (move :white "Nc3")   (move :black "c6")
        (move :white "Bg5")   (move :black "b5")
        (move :white "Nxb5")  (move :black "cxb5")
        (move :white "Bxb5+") (move :black "Nbd7")
        (move :white "0-0-0") (move :black "Rd8")
        (move :white "Rxd7")  (move :black "Rxd7")
        (move :white "Rd1")   (move :black "Qe6")
        (move :white "Bxd7+") (move :black "Nxd7")
        (move :white "Qb8+")  (move :black "Nxb8")
        (move :white "Rd8#")))

  (testing "Aronian vs. Anand, Wijk aan Zee 2013"
    (-> pieces
        (move :white "d4")   (move :black "d5")
        (move :white "c4")   (move :black "c6")
        (move :white "Nf3")  (move :black "Nf6")
        (move :white "Nc3")  (move :black "e6")
        (move :white "e3")   (move :black "Nbd7")
        (move :white "Bd3")  (move :black "dxc4")
        (move :white "Bxc4") (move :black "b5")
        (move :white "Bd3")  (move :black "Bd6")
        (move :white "0-0")  (move :black "0-0")
        (move :white "Qc2")  (move :black "Bb7")
        (move :white "a3")   (move :black "Rc8")
        (move :white "Ng5")  (move :black "c5")
        (move :white "Nxh7") (move :black "Ng4")
        (move :white "f4")   (move :black "cxd4")
        (move :white "exd4") (move :black "Bc5")
        (move :white "Be2")  (move :black "Nde5")
        (move :white "Bxg4") (move :black "Bxd4+")
        (move :white "Kh1")  (move :black "Nxg4")
        (move :white "Nxf8") (move :black "f5")
        (move :white "Ng6")  (move :black "Qf6")
        (move :white "h3")   (move :black "Qxg6")
        (move :white "Qe2")  (move :black "Qh5")
        (move :white "Qd3")  (move :black "Be3")))

  (testing "Karpov vs. Kasparov, World Championship 1985, game 16"
    (-> pieces
        (move :white "e4")   (move :black "c5")
        (move :white "Nf3")  (move :black "e6")
        (move :white "d4")   (move :black "cxd4")
        (move :white "Nxd4") (move :black "Nc6")
        (move :white "Nb5")  (move :black "d6")
        (move :white "c4")   (move :black "Nf6")
        (move :white "N1c3") (move :black "a6")
        (move :white "Na3")  (move :black "d5")
        (move :white "cxd5") (move :black "exd5")
        (move :white "exd5") (move :black "Nb4")
        (move :white "Be2")  (move :black "Bc5")
        (move :white "0-0")  (move :black "0-0")
        (move :white "Bf3")  (move :black "Bf5")
        (move :white "Bg5")  (move :black "Re8")
        (move :white "Qd2")  (move :black "b5")
        (move :white "Rad1") (move :black "Nd3")
        (move :white "Nab1") (move :black "h6")
        (move :white "Bh4")  (move :black "b4")
        (move :white "Na4")  (move :black "Bd6")
        (move :white "Bg3")  (move :black "Rc8")
        (move :white "b3")   (move :black "g5")
        (move :white "Bxd6") (move :black "Qxd6")
        (move :white "g3")   (move :black "Nd7")
        (move :white "Bg2")  (move :black "Qf6")
        (move :white "a3")   (move :black "a5")
        (move :white "axb4") (move :black "axb4")
        (move :white "Qa2")  (move :black "Bg6")
        (move :white "d6")   (move :black "g4")
        (move :white "Qd2")  (move :black "Kg7")
        (move :white "f3")   (move :black "Qxd6")
        (move :white "fxg4") (move :black "Qd4+")
        (move :white "Kh1")  (move :black "Nf6")
        (move :white "Rf4")  (move :black "Ne4")
        (move :white "Qxd3") (move :black "Nf2+")
        (move :white "Rxf2") (move :black "Bxd3")
        (move :white "Rfd2") (move :black "Qe3")
        (move :white "Rxd3") (move :black "Rc1")
        (move :white "Nb2")  (move :black "Qf2")
        (move :white "Nd2")  (move :black "Rxd1+")
        (move :white "Nxd1") (move :black "Re1+")))

  (testing "Byrne vs. Fischer, New York 1956"
    (-> pieces
        (move :white "Nf3")  (move :black "Nf6")
        (move :white "c4")   (move :black "g6")
        (move :white "Nc3")  (move :black "Bg7")
        (move :white "d4")   (move :black "0-0")
        (move :white "Bf4")  (move :black "d5")
        (move :white "Qb3")  (move :black "dxc4")
        (move :white "Qxc4") (move :black "c6")
        (move :white "e4")   (move :black "Nbd7")
        (move :white "Rd1")  (move :black "Nb6")
        (move :white "Qc5")  (move :black "Bg4")
        (move :white "Bg5")  (move :black "Na4")
        (move :white "Qa3")  (move :black "Nxc3")
        (move :white "bxc3") (move :black "Nxe4")
        (move :white "Bxe7") (move :black "Qb6")
        (move :white "Bc4")  (move :black "Nxc3")
        (move :white "Bc5")  (move :black "Rfe8+")
        (move :white "Kf1")  (move :black "Be6")
        (move :white "Bxb6") (move :black "Bxc4+")
        (move :white "Kg1")  (move :black "Ne2+")
        (move :white "Kf1")  (move :black "Nxd4+")
        (move :white "Kg1")  (move :black "Ne2+")
        (move :white "Kf1")  (move :black "Nc3+")
        (move :white "Kg1")  (move :black "axb6")
        (move :white "Qb4")  (move :black "Ra4")
        (move :white "Qxb6") (move :black "Nxd1")
        (move :white "h3")   (move :black "Rxa2")
        (move :white "Kh2")  (move :black "Nxf2")
        (move :white "Re1")  (move :black "Rxe1")
        (move :white "Qd8+") (move :black "Bf8")
        (move :white "Nxe1") (move :black "Bd5")
        (move :white "Nf3")  (move :black "Ne4")
        (move :white "Qb8")  (move :black "b5")
        (move :white "h4")   (move :black "h5")
        (move :white "Ne5")  (move :black "Kg7")
        (move :white "Kg1")  (move :black "Bc5+")
        (move :white "Kf1")  (move :black "Ng3+")
        (move :white "Ke1")  (move :black "Bb4+")
        (move :white "Kd1")  (move :black "Bb3+")
        (move :white "Kc1")  (move :black "Ne2+")
        (move :white "Kb1")  (move :black "Nc3+")
        (move :white "Kc1")  (move :black "Rc2#")))

  (testing "Chess promotion"
    (-> pieces
        (move :white "b4")      (move :black "c6")
        (move :white "b5")      (move :black "Qb6")
        (move :white "bxc6")    (move :black "Qd4")
        (move :white "cxb7")    (move :black "Qxa1")
        (move :white "bxc8=R#")))

  (testing "En passant"
    (-> pieces
        (move :white "e4")   (move :black "e6")
        (move :white "e5")   (move :black "d5")
        (move :white "exd6"))))
