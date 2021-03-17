(ns santorini.utils)

;; Defines a Move record. The purpose of this record
;; is to keep track of all of the information regarding
;; a particular move
;; 
;; pieces: a 2 dimensional array of players,
;;   it holds the positions of all 4 tokens
;; pieceNum: the index of the piece that we are currently moving
;; pieceStart: the position which the piece started in
;; startHeight: the starting height of the piece
;; currentHeight: the height of the piece in its current position
;; hasMovedUp: keeps track of whether we have moved to a higher level than we started
;; builds: an array of builds representing the builds we have made so far
;; card: a string representing the god card
(defrecord Move [pieces
                 pieceNum
                 pieceStart
                 startHeight
                 currentHeight
                 hasMovedUp
                 builds
                 card])

;; A build record keeps track of the position on the board which we want to build
;; on as well as the new height of that position (after building)
;; 
;; pos: an array of 2 numbers representing the position on the board where 
;;   we are building
;; newHeight: the height of the new build
(defrecord Build [pos
                  newHeight])

(defn get-move-pieces
  [move]
  (:pieces move))

(defn get-move-piecePos
  [move]
  (get-in (:pieces move) [0 (:pieceNum move)]))

(defn get-move-pieceNum
  [move]
  (:pieceNum move))

(defn get-move-pieceStart
  [move]
  (:pieceStart move))

(defn get-move-startHeight
  [move]
  (:startHeight move))

(defn get-move-currentHeight
  [move]
  (:currentHeight move))

(defn get-move-hasMovedUp
  [move]
  (:hasMovedUp move))

(defn get-move-card
  [move]
  (:card move))

(defn get-move-builds
  [move]
  (vec (:builds move)))

(defn get-build-pos
  [build]
  (:pos build))

(defn get-build-newHeight
  [build]
  (:newHeight build))

(defn set-move-pieces
  [move pieces]
  (assoc move :pieces pieces))

(defn set-move-pieceNum
  [move pieces]
  (assoc move :pieceNum pieces))

(defn set-move-pieceStart
  [move pieces]
  (assoc move :pieceStart pieces))

(defn set-move-startHeight
  [move pieces]
  (assoc move :startHeight pieces))

(defn set-move-currentHeight
  [move pieces]
  (assoc move :currentHeight pieces))

(defn set-move-hasMovedUp
  [move pieces]
  (assoc move :hasMovedUp pieces))

(defn set-move-builds
  [move pieces]
  (assoc move :builds pieces))

(defn set-move-card
  [move card]
  (assoc move :card card))

(defn set-build-pos
  [build pos]
  (assoc build :pos pos))

(defn set-build-newHeight
  [build newHeight]
  (assoc build :newHeight newHeight))

(defn append-build
  [move build]
  (set-move-builds move (conj (get-move-builds move) build)))

(defn move-has-duplicate-build?
  [move]
  (let [builds (get-move-builds move)]
    (if (< (count builds) 2)
      false
      (= (get-build-pos (first builds)) (get-build-pos (second builds))))))

(defn update-players
  [players pos newloc]
  (assoc-in players [0 pos] newloc))

(defn set-move-piecePos
  [move pos]
  (assoc move :pieces (update-players
                       (get-move-pieces move)
                       (get-move-pieceNum move)
                       pos)))

(defn has-moved-up?
  [move new-height]
  (or (get-move-hasMovedUp move) (> new-height (get-move-currentHeight move))))

(defn get-board-from-map
  [json-map]
  (get json-map "spaces"))

(defn get-turn-from-map
  [json-map]
  (get json-map "turn"))

(defn get-players-from-map
  [json-map]
  (get json-map "players"))

(defn get-player-val
  [players kw idx]
  (get (get players idx) kw))

(defn extract-from-players
  [players kw]
  (let [l []
        l (assoc l 0 (get-player-val players kw 0))
        l (assoc l 1 (get-player-val players kw 1))]
    l))

(defn get-tokens-from-map
  [players]
  (extract-from-players players "tokens"))

(defn get-cards-from-map
  [players]
  (extract-from-players players "card"))