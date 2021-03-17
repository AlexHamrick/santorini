(ns santorini.core
  (:gen-class)
  (:require [cheshire.core :refer :all :as cheshire])
  (:require [santorini.utils :as u])
  (:require [santorini.utils :refer (->Move)])
  (:require [santorini.utils :refer (->Build)]))

(declare initial-setup
         main-loop
         pick-start-pos
         create-new-board
         create-init-player
         create-new-players
         pick-move
         compare-moves
         take-turn
         take-turn-generic
         artemis-turn
         atlas-turn
         hephastus-turn
         has-won?
         define-blank-move
         get-valid-moves
         get-valid-builds
         within-one?
         add-coords
         movable-height?
         read-json
         move-to-json)

(defn -main
  []
  ;; Select initial token positions
  (initial-setup)
  ;; Game loop- take in board state and output state after our move
  (while true
    (main-loop)))

(defn initial-setup
  []
  (let [json (read-line)
        jmap (vec (read-json json))
        new-pos (pick-start-pos jmap)
        player-added (create-init-player jmap new-pos)]
    (println (cheshire/generate-string player-added))
    (flush)))

(defn main-loop
  []
  (let [json (read-line)
        jmap (read-json json)
        board (u/get-board-from-map jmap)
        turn (u/get-turn-from-map jmap)
        players (u/get-players-from-map jmap)
        tokens (u/get-tokens-from-map players)
        cards (u/get-cards-from-map players)
        player-card (first cards)
        options (take-turn board tokens player-card)
        move (pick-move options)
        result (move-to-json move board players turn)
        newjson (cheshire/generate-string result)]
    (println newjson)
    (flush)))

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
  ;; (if (< (u/get-move-currentHeight move) 3)
    (let [build (get (u/get-move-builds move) 0)]
      (if (or (nil? build)
              (empty? build))
        board
        (let [bpos (u/get-build-pos build)] 
          (println move)
             (println build)
             (println bpos)
             (assoc-in board (vec (map dec bpos))
                       (u/get-build-newHeight build))
          ;; ]
             )
        ))
    ;; board)
  )

(defn create-init-player
  [players tokens]
  (let [flip-players [(get players 1) (get players 0)]
        second (get flip-players 1)
        adjusted-second (assoc second "tokens" tokens)]
    (assoc flip-players 1 adjusted-second)))

(defn create-new-players
  [players move]
  (let [flip-players [(get players 1) (get players 0)]]
    (assoc-in flip-players [1 "tokens" (u/get-move-pieceNum move)] (u/get-move-piecePos move))))

(defn pick-move
  [moves]
  (rand-nth (vec (sort compare-moves (vec moves)))))
  ;; (get (vec (sort compare-moves (vec moves))) 0))

(defn compare-moves
  [move1 move2]
  ;; (println move1 move2)
  (let [mh1 (u/get-move-currentHeight move1)
        mh2 (u/get-move-currentHeight move2)
        win1 (has-won? move1 (u/get-move-card move1))
        win2 (has-won? move2 (u/get-move-card move2))
        ;; build1 (:pos (get (u/get-move-builds move1) 0))
        ;; build2 (:pos (get (u/get-move-builds move2) 0))
        c1 (compare (- 0 mh1) (- 0 mh2))
        ;; c2 (compare (add-coords build1) (add-coords build2))
        ]
    (if (= win1 win2)
      c1
      (if win1
        -1
        1))
    ;; (if (zero? c1)
    ;;   c2
    ;;   c1)
    ;; c1
    ))

(defn take-turn 
  [board players card]
  (if (= card "Artemis")
    (artemis-turn board players card)
    (let [generic (take-turn-generic board players card)]
      (if (= card "Atlas")
        (vec (concat generic (atlas-turn generic)))
        (if (= card "Hephastus")
          (vec (concat generic (hephastus-turn generic)))
          generic))
      ))
    ;;if we are none of the specified cards
  )

;;TODO test that atlas doesn't mess up when there are no builds
(defn atlas-turn
  [moves]
  (let [new-heights (for [mv moves]
                      (let [builds (u/get-move-builds mv)]
                        (when (and (seq builds)
                                   (< (u/get-build-newHeight (first builds))  4))
                          (u/set-move-builds mv [(u/set-build-newHeight
                                                 (first builds) 4)]
                          ))))]
    (remove nil? (vec new-heights))
    )
  )

(defn hephastus-turn
  [moves]
  (let [new-heights (for [mv moves]
                      (let [builds (u/get-move-builds mv)]
                        (when (and (seq builds)
                                   (< (u/get-build-newHeight (first builds))  3))
                          (u/set-move-builds mv [(u/set-build-newHeight
                                                  (first builds)
                                                  (inc (u/get-build-newHeight (first builds))))]
                          ))))]
    (remove nil? (vec new-heights))))


(defn artemis-turn 
  [board players card]
    (let [res (for [pos (range 2)
                  :let [start-move (define-blank-move board players pos card)
                        moves (for [mv1 (get-valid-moves board start-move)
                                    mv2 (get-valid-moves board mv1)
                                    build (get-valid-builds board mv2)]
                                (if (has-won? mv1 card)
                                  mv1
                                  (if (has-won? mv2 card)
                                    mv2
                                    build))
                              )]]
              moves)]
    (vec (flatten res)))
  )

(defn take-turn-generic
  [board players card]
  (let [res (for [pos (range 2)
                  :let [start-move (define-blank-move board players pos card)
                        moves (for [mv (get-valid-moves board start-move)
                                    build (get-valid-builds board mv)]
                                (if (has-won? mv card)
                                  mv
                                  build))]]
              moves)]
    (vec (flatten res))))

(defn define-blank-move
  [board players piece-num card]
  (let [moving-piece (get-in players [0 piece-num])
        adj-pos (map dec moving-piece)
        height (get-in board adj-pos)]
    (->Move players
            piece-num
            moving-piece
            height
            height
            false
            []
            card)))

(defn get-valid-moves
  [board move]
  (let [tokens (u/get-move-pieces move)
        piece (u/get-move-piecePos move)
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
                              (not (some #(= [(inc x) (inc y)] %) player2))
                              (not= [(inc x) (inc y)] (u/get-move-pieceStart move)))]
               (let [move (u/set-move-piecePos move [(inc x) (inc y)])
                     move (u/set-move-hasMovedUp move (u/has-moved-up? move val))
                     move (u/set-move-currentHeight move val)]
                 move))]
    (vec vals)))

(defn get-valid-builds
  [board move]
  (let [tokens (u/get-move-pieces move)
        piece (u/get-move-piecePos move)
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
                              (not (some #(= [(inc x) (inc y)] %) player2)))]
               (let [bld (->Build [(inc x) (inc y)] (inc val))
                     move (u/append-build move bld)]
                 move))]
    (vec vals)))

(defn has-won?
  [move card]
  (if (and (= (u/get-move-currentHeight move) 3)
           (u/get-move-hasMovedUp move))
    true
    (if (and (= card "Pan")
             (< 1 (- (u/get-move-startHeight move) (u/get-move-currentHeight move))))
      true
      false)
    )
  )

(defn within-one?
  [num1 num2]
  (not (> (Math/abs (- num1 num2)) 1)))

(defn movable-height?
  [level target]
  (<= (- target level) 1))

(defn add-coords
  [coord]
  (+ (get coord 0) (get coord 1)))

(defn read-json
  [json]
  (cheshire/parse-string json))

(defn move-to-json
  [move board players turn]
  (let [turnmap (assoc {} "turn" (inc turn))
        pmap (assoc turnmap "players" (create-new-players players move))
        bmap (assoc pmap "spaces" (create-new-board board move))]
    bmap))
