(ns santorini.core
  (:gen-class)
  (:require [cheshire.core :refer :all :as cheshire]))

(declare
 main-loop
 initial-setup
 within-one?
 movable-height?
 get-valid-builds
 get-valid-moves
 update-players
 read-json
 get-players-from-map
 get-board-from-map
 get-turn-from-map
 get-tokens-from-map
 get-cards-from-map
 get-player-val
 extract-from-players
 pick-start-pos
 take-turn
 define-blank-move
 pick-move
 get-selected-token
 get-move
 get-build
 get-move-height
 get-build-height


 get-move-pieces
 get-move-pieceNum
 get-move-pieceStart
 get-move-startHeight
 get-move-currentHeight
 get-move-hasMovedUp
 get-move-builds
 get-move-piecePos
 get-build-pos
 get-build-newHeight

 set-move-pieces
 set-move-pieceNum
 set-move-pieceStart
 set-move-startHeight
 set-move-currentHeight
 set-move-hasMovedUp
 set-move-builds
 set-move-piecePos
 append-build
 has-moved-up?

 add-coords
 compare-moves
 move-to-json
 create-new-board
 create-new-players
 create-init-player)

(defrecord Move [pieces 
                 pieceNum 
                 pieceStart
                 startHeight 
                 currentHeight
                 hasMovedUp
                 builds])

(defrecord Build [pos
                  newHeight])

(defn -main
  []
  ;; First process the cards and select initial positions
  ;; (let [json (read-line)
  ;;       jmap (vec (read-json json))
  ;;       new-pos (pick-start-pos jmap)
  ;;       player-added (create-init-player jmap new-pos)]
  ;;   (println (cheshire/generate-string player-added))
  ;;   (flush))
  (initial-setup)     

  ;; Game loop- take in board state and output state after our move
  (while true
    ;; (let [json (read-line)
    ;;       jmap (read-json json)
    ;;       board (get-board-from-map jmap)
    ;;       turn (get-turn-from-map jmap)
    ;;       players (get-players-from-map jmap)
    ;;       tokens (get-tokens-from-map players)
    ;;       cards (get-cards-from-map players)
    ;;       player-card (first cards)
    ;;       options (take-turn board tokens turn player-card)
    ;;       move (pick-move options)
    ;;       result (move-to-json move board players turn)
    ;;       newjson (cheshire/generate-string result)]
    ;;   (println newjson)
    ;;   (flush))

    (main-loop)))

(defn initial-setup
  []
  (let [json (read-line)
        jmap (vec (read-json json))
        new-pos (pick-start-pos jmap)
        player-added (create-init-player jmap new-pos)]
    (println (cheshire/generate-string player-added))
    (flush))
  )

(defn main-loop
  []
  (let [json (read-line)
        jmap (read-json json)
        board (get-board-from-map jmap)
        turn (get-turn-from-map jmap)
        players (get-players-from-map jmap)
        tokens (get-tokens-from-map players)
        cards (get-cards-from-map players)
        player-card (first cards)
        options (take-turn board tokens turn player-card)
        move (pick-move options)
        result (move-to-json move board players turn)
        newjson (cheshire/generate-string result)]
    (println newjson)
    (flush))
  )


(defn pick-start-pos
  [json]
  (if (contains? (get (vec json) 1) "tokens")
    (let [taken (get-in (vec json) [1 "tokens"])
          ideal [[3 2] [4 4] [2 3] [4 3]]
          reduced-ideal (remove (set taken) ideal)]
      (vec (take 2 reduced-ideal)))
    ;; If nothing has been chosen yet
    [[3 2] [4 4]]))



(defn create-new-board
  [board move]
  (if (< (get-move-currentHeight move) 3)
    (let [build (get (get-move-builds move) 0)]
       (assoc-in board (vec (map dec (get-build-pos build))) 
                 (inc (get-build-newHeight build))))
   
    board))

(defn create-init-player
  [players tokens]
  (let [flip-players [(get players 1) (get players 0)]
        second (get flip-players 1)
        adjusted-second (assoc second "tokens" tokens)]
    (assoc flip-players 1 adjusted-second)
    ;; (assoc flip-players [1 "tokens"] tokens)
    ))

(defn create-new-players
  [players move]
  (let [flip-players [(get players 1) (get players 0)]]
    (assoc-in flip-players [1 "tokens" (get-move-pieceNum move)] (get-move-piecePos move))))

(defn pick-move
  [moves] 
  ;; (println moves)
  (get (vec (sort compare-moves (vec moves))) 0))

(defn compare-moves
  [move1 move2]
  ;; (println (get-move-currentHeight move1))
  ;; (println move1)
  ;; (println (get-move-currentHeight move2))
  (let [mh1 (get-move-currentHeight move1)
        mh2 (get-move-currentHeight move2)

        build1 (:pos (get (get-move-builds move1) 0))
        build2 (:pos (get (get-move-builds move2) 0))
        c1 (compare (- 0 mh1) (- 0 mh2))
        c2 (compare (add-coords build1) (add-coords build2))]
    (if (zero? c1)
      c2
      c1)))

(defn add-coords
  [coord]
  (+ (get coord 0) (get coord 1)))

(defn extract-from-players
  [players kw]
  (let [l []
        l (assoc l 0 (get-player-val players kw 0))
        l (assoc l 1 (get-player-val players kw 1))]
    l))




;; (defn take-turn
;;   [board players turn card]
;;   (let [res (for [pos (range 2)
;;                   move (get-valid-moves board (get-in players [0 pos]) players)
;;                   build (get-valid-builds board (get-selected-token move) (update-players players pos move))]
;;               [pos move build])]
;;     (vec res)))


(defn take-turn
  [board players turn card]
  (let [res (for [pos (range 2)
                  :let [start-move (define-blank-move board players pos)
                        moves (for [mv (get-valid-moves board start-move)
                                    build (get-valid-builds board mv)]
                                build)
                      ]
                  ]
              moves
            )]
    (vec (flatten res))))

(defn define-blank-move
  [board players piece-num]
  (let [moving-piece (get-in players [0 piece-num])
        adj-pos (map dec moving-piece)
        height (get-in board adj-pos)]
    (->Move players
            piece-num
            moving-piece
            height
            height
            false
            [])))

(defn update-players
  [players pos newloc]
  ;; (println (assoc-in players [0 pos] newloc))
  (assoc-in players [0 pos] newloc))


(defn get-valid-moves
  [board move]
  (let [tokens (get-move-pieces move)
        piece (get-move-piecePos move)
        dec-pos (vec (map dec piece))
        xpos (get dec-pos 0)
        ypos (get dec-pos 1)
        player1 (get tokens 0)
        player2 (get tokens 1)
        level (get-in board [xpos ypos])
        vals (for [[x row] (map-indexed vector board)
                   [y val] (map-indexed vector row)
                   :when (and (movable-height? level val)
                              (within-one? x xpos)
                              (within-one? y ypos)
                              (or (not= x xpos) (not= y ypos))
                              (not (some #(= [(inc x) (inc y)] %) player1))
                              (not (some #(= [(inc x) (inc y)] %) player2)))]
               (let [move (set-move-piecePos move [(inc x) (inc y)])
                     move (set-move-hasMovedUp move (has-moved-up? move val))
                     move (set-move-currentHeight move val)]
                 move))]
    (vec vals)
    ))



(defn get-valid-builds
  [board move]
  (let [tokens (get-move-pieces move)
        piece (get-move-piecePos move)
        dec-pos (vec (map dec piece))
        xpos (get dec-pos 0)
        ypos (get dec-pos 1)
        player1 (get tokens 0)
        player2 (get tokens 1)
        vals (for [[x row] (map-indexed vector board)
                   [y val] (map-indexed vector row)
                   :when (and (< val 4)
                              (within-one? x xpos)
                              (within-one? y ypos)
                              (or (not= x xpos) (not= y ypos))
                              (not (some #(= [(inc x) (inc y)] %) player1))
                              (not (some #(= [(inc x) (inc y)] %) player2))
                              )]
               (let [bld (->Build [(inc x) (inc y)] val)
                     move (append-build move bld)]
                 move))]
    (vec vals)))
;;
;; MUST HAVE THE CORRECT PIECES
;; (defn get-valid-builds
;;   [board piece players]
;;   (let [xpos (dec (get piece 0))
;;         ypos (dec (get piece 1))
;;         player1 (get players 0)
;;         player2 (get players 1)
;;         vals (for [[x row] (map-indexed vector board)
;;                    [y val] (map-indexed vector row)
;;                    :when (and (< val 4)
;;                               (within-one? x xpos)
;;                               (within-one? y ypos)
;;                               (or (not= x xpos) (not= y ypos))
;;                               (not (some #(= [(inc x) (inc y)] %) player1))
;;                               (not (some #(= [(inc x) (inc y)] %) player2))
;;                               )]
;;                [[(inc x) (inc y)] val])]
;;     (vec vals)))



(defn within-one?
  [num1 num2]
  (not (> (Math/abs (- num1 num2)) 1)))

(defn movable-height?
  [level target]
  (<= (- target level) 1))

(defn get-move-pieces
  [move]
  (:pieces move))

(defn get-move-piecePos
  [move]
  ;; (println move)
  ;; (println (get-in (:pieces move) [0 (:pieceNum move)]))
  (get-in (:pieces move) [0 (:pieceNum move)]))

(defn get-move-pieceNum
  [move]
  (:pieceNum move))

(defn get-move-pieceStart
  [move]
  (:pieces move))

(defn get-move-startHeight
  [move]
  (:startHeight move))

(defn get-move-currentHeight
  [move]
  (:currentHeight move))

(defn get-move-hasMovedUp
  [move]
  (:hasMovedUp move))

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

(defn append-build
  [move build]
  (set-move-builds move (conj (get-move-builds move) build)))

(defn set-move-piecePos
  [move pos]
  (assoc move :pieces (update-players 
                       (get-move-pieces move) 
                       (get-move-pieceNum move) 
                       pos)))

(defn has-moved-up?
  [move new-height]
    (or (get-move-hasMovedUp move) (> new-height (get-move-currentHeight move))))

;; (defn get-move
;;   [move]
;;   (get (get move 1) 0))

;; (defn get-selected-token
;;   [move]
;;   (:pieceNum move))

;; (defn get-move-height
;;   [move]
;;   (get (get move 1) 1))

;; (defn get-build
;;   [move]
;;   (get (get move 2) 0))

;; (defn get-build-height
;;   [move]
;;   (get (get move 2) 1))



(defn get-player-val
  [players kw idx]
  (get (get players idx) kw))





(defn get-board-from-map
  [json-map]
  (get json-map "spaces"))

(defn get-turn-from-map
  [json-map]
  (get json-map "turn"))

(defn get-players-from-map
  [json-map]
  (get json-map "players"))


(defn get-tokens-from-map
  [players]
  (extract-from-players players "tokens"))

(defn get-cards-from-map
  [players]
  (extract-from-players players "card"))

(defn read-json
  [json]
  (cheshire/parse-string json))

(defn move-to-json
  [move board players turn]
  (let [turnmap (assoc {} "turn" (inc turn))
        pmap (assoc turnmap "players" (create-new-players players move))
        bmap (assoc pmap "spaces" (create-new-board board move))]
    bmap))