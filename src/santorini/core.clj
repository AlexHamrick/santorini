(ns santorini.core
  (:gen-class)
  (:require [cheshire.core :refer :all :as cheshire]))


(declare within-one?
         movable-height?
         get-valid-builds
         get-valid-moves
         update-players
         read-json
         get-players
         get-board
         get-turn
         get-tokens
         get-cards
         get-player-val
         extract-from-players
         pick-start-pos
         take-turn
         pick-move
         get-selected-token
         get-move
         get-build
         get-move-height
         get-build-height
         add-coords
         compare-moves
         move-to-json
         create-new-board
         create-new-players
         create-init-player)

(defn -main
  []
  (let [
        ;; json (line-seq (java.io.BufferedReader. *in*))
        ;; json (slurp *in*)
        json (read-line)
        jmap (vec (read-json json))
        new-pos (pick-start-pos jmap)
        player-added (create-init-player jmap new-pos)]
    (println (cheshire/generate-string player-added))
    (flush))

  (while true
    (let [
          ;; json (slurp *in*)
          ;; json (line-seq (java.io.BufferedReader. *in*))
          json (read-line)
          jmap (read-json json)
          board (get-board jmap)
          turn (get-turn jmap)
          players (get-players jmap)
          tokens (get-tokens players)
          cards (get-cards players)
          player-card (first cards)
          options (take-turn board tokens turn player-card)
          move (pick-move options)
          result (move-to-json move board players turn)
          newjson (cheshire/generate-string result)]
      (println newjson)
      (flush))))

(defn pick-start-pos
  [json]
  (if (contains? (get (vec json) 1) "tokens")
    (let [taken (get-in (vec json) [1 "tokens"])
          ideal [[3 2] [4 4] [2 3] [4 3]]
          reduced-ideal (remove (set taken) ideal)]
      (vec (take 2 reduced-ideal)))
    [[3 2] [4 4]]))

(defn is-a-win 
  [])

(defn move-to-json
  [move board players turn]
  (let [turnmap (assoc {} "turn" (inc turn))
        pmap (assoc turnmap "players" (create-new-players players move))
        bmap (assoc pmap "spaces" (create-new-board board move))]
    bmap))

(defn create-new-board
  [board move]
  (if (< (get-move-height move) 3)
    (assoc-in board (vec (map dec (get-build move))) (inc (get-build-height move)))
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
    (assoc-in flip-players [1 "tokens" (get-selected-token move)] (get-move move))))

(defn pick-move
  [options]
  (get (vec (sort compare-moves options)) 0))

(defn compare-moves
  [move1 move2]
  (let [mh1 (get-move-height move1)
        mh2 (get-move-height move2)
        build1 (get-build move1)
        build2 (get-build move2)
        c1 (compare (- 0 mh1) (- 0 mh2))
        c2 (compare (add-coords build1) (add-coords build2))]
    (if (zero? c1)
      c2
      c1)))

(defn add-coords
  [coord]
  (+ (get coord 0) (get coord 1)))

(defn get-move
  [move]
  (get (get move 1) 0))

(defn get-selected-token
  [move]
  (get move 0))

(defn get-move-height
  [move]
  (get (get move 1) 1))

(defn get-build
  [move]
  (get (get move 2) 0))

(defn get-build-height
  [move]
  (get (get move 2) 1))

(defn get-players
  [json-map]
  (get json-map "players"))


(defn get-tokens
  [players]
  (extract-from-players players "tokens"))

(defn get-cards
  [players]
  (extract-from-players players "card"))

(defn get-player-val
  [players kw idx]
  (get (get players idx) kw))

(defn extract-from-players
  [players kw]
  (let [l []
        l (assoc l 0 (get-player-val players kw 0))
        l (assoc l 1 (get-player-val players kw 1))]
    l))

(defn get-board
  [json-map]
  (get json-map "spaces"))

(defn get-turn
  [json-map]
  (get json-map "turn"))

(defn read-json
  [json]
  (cheshire/parse-string json))


(defn take-turn
  [board players turn card]
  (let [res (for [pos (range 2)
                  move (get-valid-moves board (get-in players [0 pos]) players)
                  build (get-valid-builds board (get-selected-token move) (update-players players pos move))]
              [pos move build])]
    (vec res)))

(defn update-players
  [players pos newloc]
  ;; (println (assoc-in players [0 pos] newloc))
  (assoc-in players [0 pos] newloc))

(defn get-valid-moves
  [board piece players]
  (let [xpos (dec (get piece 0))
        ypos (dec (get piece 1))
        player1 (get players 0)
        player2 (get players 1)
        level (get-in board [xpos ypos])
        vals (for [[x row] (map-indexed vector board)
                   [y val] (map-indexed vector row)
                   :when (and (movable-height? level val)
                              (within-one? x xpos)
                              (within-one? y ypos)
                              (or (not= x xpos) (not= y ypos))
                              (not (some #(= [(inc x) (inc y)] %) player1))
                              (not (some #(= [(inc x) (inc y)] %) player2))
                              true)]
               [[(inc x) (inc y)] val])]
    (vec vals)))

;;
;; MUST HAVE THE CORRECT PIECES
(defn get-valid-builds
  [board piece players]
  (let [xpos (dec (get piece 0))
        ypos (dec (get piece 1))
        player1 (get players 0)
        player2 (get players 1)
        vals (for [[x row] (map-indexed vector board)
                   [y val] (map-indexed vector row)
                   :when (and (< val 4)
                              (within-one? x xpos)
                              (within-one? y ypos)
                              (or (not= x xpos) (not= y ypos))
                              (not (some #(= [(inc x) (inc y)] %) player1))
                              (not (some #(= [(inc x) (inc y)] %) player2))
                              true)]
               [[(inc x) (inc y)] val])]
    (vec vals)))



(defn within-one?
  [num1 num2]
  (not (> (Math/abs (- num1 num2)) 1)))

(defn movable-height?
  [level target]
  (<= (- target level) 1))