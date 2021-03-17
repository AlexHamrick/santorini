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
         set-build-in-board
         create-init-player
         create-new-players
         pick-move
         compare-moves
         take-turn
         take-turn-generic
         artemis-turn
         atlas-turn
         hephastus-turn
         demeter-turn
         prometheus-turn
         nil-or-duplicate-builds?
         has-won?
         define-blank-move
         get-valid-moves
         get-valid-builds
         within-one?
         add-coords
         movable-height?
         read-json
         move-to-json)

;; The main game function
(defn -main
  []
  ;; Select initial token positions
  (initial-setup)
  ;; Game loop- take in board state and output state after our move
  (while true
    (main-loop)))

;; Reads a line of JSON representing the token selection.
;; Prints a line of JSON with our added token selection
(defn initial-setup
  []
  (let [json (read-line)
        jmap (vec (read-json json))
        new-pos (pick-start-pos jmap)
        player-added (create-init-player jmap new-pos)]
    (println (cheshire/generate-string player-added))
    (flush)))

;; Reads JSON representing the board state, makes a move, and prints 
;; the appropriate JSON to match our move
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
        ;; Generate all of the possible moves
        options (take-turn board tokens player-card)
        ;; select the most desirable move 
        move (pick-move options)
        result (move-to-json move board players turn)
        newjson (cheshire/generate-string result)]
    (println newjson)
    (flush)))

;; Selects our starting token positions
;; Simply picks the first two available positions
;; from the array [[3 2] [4 4] [2 3] [4 3]]
(defn pick-start-pos
  [json]
  (if (contains? (get (vec json) 1) "tokens")
    ;; If the other player has selected their token positions
    (let [taken (get-in (vec json) [1 "tokens"])
          ideal [[3 2] [4 4] [2 3] [4 3]]
          reduced-ideal (remove (set taken) ideal)]
      (vec (take 2 reduced-ideal)))
    ;; If nothing has been chosen yet
    [[3 2] [4 4]]))

;; Creates the board associated with a move.
;; Essentially, just increment the board positions
;; where we have decided to build
(defn create-new-board
  [board move]
  (let [builds (u/get-move-builds move)]
    (if (empty? builds)
        ;; If we built nothing, then return the same board
      board
        ;; If we built at least one thing, try building up to 2
      (let [build1 (first builds)
            build2 (second builds)]
        (set-build-in-board (set-build-in-board board build1) build2)))))

;; Increments the position of a board corresponding to the passed build
(defn set-build-in-board
  [board build]
  (if (nil? build)
    board
    (assoc-in board (vec (map dec (u/get-build-pos build)))
              (u/get-build-newHeight build))))

;; Creates the new player map from the set of tokens and the initial 
;; player passed via json.
;; Used only in initial-setup phase.
(defn create-init-player
  [players tokens]
  (let [;; Swap the order of the players
        flip-players [(second players) (first players)]
        ;; Insert the new token positions into the map as tokens
        adjusted-second (assoc (second flip-players) "tokens" tokens)]
    (assoc flip-players 1 adjusted-second)))

;; Creates the new player map from the set of tokens and the initial 
;; player passed via json.
;; Used only in main-loop phase.
(defn create-new-players
  [players move]
  (let [flip-players [(second players) (first players)]]
    ;; set the player tokens to be those defined by the move
    (assoc-in flip-players [1 "tokens" (u/get-move-pieceNum move)] (u/get-move-piecePos move))))

;; Selects a move from a list of moves.
;; Currently, we sort based on compare-moves and select the first element.
(defn pick-move
  [moves]
  ;; Uncomment to try random moves and ensure we aren't doing anything funny with moves that aren't taken
  ;; (rand-nth (vec (sort compare-moves moves))))
  (first (vec (sort compare-moves moves))))

;; Determines which move is best between two
;; Currently, we just take the move which wins, otherwise we prioritize
;; token height
(defn compare-moves
  [move1 move2]
  (let [mh1 (u/get-move-currentHeight move1)
        mh2 (u/get-move-currentHeight move2)
        win1 (has-won? move1 (u/get-move-card move1))
        win2 (has-won? move2 (u/get-move-card move2))
        c1 (compare (- mh1) (- mh2))]
    (if (= win1 win2)
      c1
      (if win1
        -1
        1))))

;; Generate all of the possible moves when given a board, set of tokens, and card to move
;; Currently, this function does not support moves that move the opposing player
;; (such as the special moves defined by Apollo and Minotaur)
(defn take-turn
  [board players card]
  (if (= card "Artemis")
    (artemis-turn board players card)
    (if (= card "Prometheus")
      (prometheus-turn board players card)
      (let [generic (take-turn-generic board players card)]
        (if (= card "Atlas")
          (vec (concat generic (atlas-turn generic)))
          (if (= card "Hephastus")
            (vec (concat generic (hephastus-turn generic)))
            (if (= card "Demeter")
              (vec (concat generic (demeter-turn board generic)))
              ;; If we are none of the specified cards, simply return 
              ;; the default move set
              generic)))))))

;; Defines the additional moves that atlas can perform over the generic set.
;; Atlas is able to build any platform up to a height of 4
;; @param moves The set of generic moves that anyone can perform
(defn atlas-turn
  [moves]
  (let [new-heights (for [mv moves]
                      (let [builds (u/get-move-builds mv)]
                        (when (and (seq builds)
                                   (< (u/get-build-newHeight (first builds))  4))
                          (u/set-move-builds mv [(u/set-build-newHeight
                                                  (first builds) 4)]))))]
    (remove nil? (vec new-heights))))

;; Defines the additional moves that hephastus can perform over the generic set.
;; Hephastus can build on the same location twice (increasing its height by 2)
;; @param moves The set of generic moves that anyone can perform
(defn hephastus-turn
  [moves]
  (let [new-heights (for [mv moves]
                      (let [builds (u/get-move-builds mv)]
                        (when (and (seq builds)
                                   (< (u/get-build-newHeight (first builds))  3))
                          (u/set-move-builds mv [(u/set-build-newHeight
                                                  (first builds)
                                                  (inc (u/get-build-newHeight (first builds))))]))))]
    (remove nil? (vec new-heights))))

;; Defines the additional moves that demeter can perform over the generic set.
;; Demeter can build twice, as long as she isn't building on the same block both times.
;; @param moves The set of generic moves that anyone can perform
(defn demeter-turn
  [board moves]
  (let [added-builds (for [mv moves]
                       (let [builds (u/get-move-builds mv)]
                         (when (seq builds)
                           (get-valid-builds board mv))))]
    (remove nil-or-duplicate-builds? (flatten added-builds))))

;; Determines whether a move is nil or has two builds with the same position.
;; This method is used when generatign demeters possible moves, since we need to 
;; eliminate all the moves for which we are building in the same place twice
(defn nil-or-duplicate-builds?
  [move]
  (if (nil? move)
    true
    (u/move-has-duplicate-build? move)))

;; Generates all the moves that prometheus can make given a board and set of tokens
;; Allows prometheus to build before moving as long as his move doesn't go up a level
(defn prometheus-turn
  [board players card]
  (let [res (for [pos (range 2)
                  :let [start-move (define-blank-move board players pos card)
                        moves (for [;; conj the start move to ensure that we don't 
                                    ;; have to build before moving
                                    build1 (conj (get-valid-builds board start-move) start-move)
                                    mv (get-valid-moves board build1)
                                    build2 (get-valid-builds board mv)]
                                (if (has-won? mv card)
                                  mv
                                  build2))]]
              moves)]
    (vec (flatten res))))

;; Generates all the moves that Artemis can make given a board and set of tokens.
;; Allows artemis to move twice before building
(defn artemis-turn
  [board players card]
  (let [res (for [pos (range 2)
                  :let [start-move (define-blank-move board players pos card)
                        moves (for [mv1 (get-valid-moves board start-move)
                                    ;; conj the start move to ensure that we dont HAVE to move twice
                                    mv2 (get-valid-moves board (conj mv1 start-move))
                                    build (get-valid-builds board mv2)]
                                (if (has-won? mv1 card)
                                  mv1
                                  (if (has-won? mv2 card)
                                    mv2
                                    build)))]]
              moves)]
    (vec (flatten res))))

;; Defines the generic moves accessible by everyone (defined by a move 
;; followed by a build)
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

;; Creates a new move record given a board, player tokens, piece index (0 or 1)
;; and the card
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

;; Get all the valid moves given a board and the move so far
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
        vals (for [;; Map-indexed returns both the key and values for an item
                   ;;  which are then stored as x and row respectively
                   [x row] (map-indexed vector board)
                   [y val] (map-indexed vector row)
                   ;; checks for valid moves
                   :when (and (movable-height? level val move)
                              (within-one? x xpos)
                              (within-one? y ypos)
                              (or (not= x xpos) (not= y ypos))
                              ;; If any of the elements of players match [(inc x) (inc y)]
                              ;;  Then the move is invalid
                              (not (some #(= [(inc x) (inc y)] %) player1))
                              (not (some #(= [(inc x) (inc y)] %) player2))
                              (not= [(inc x) (inc y)] (u/get-move-pieceStart move)))]
               ;; If the move is valid, set the new move fields appropriately
               (let [move (u/set-move-piecePos move [(inc x) (inc y)])
                     move (u/set-move-hasMovedUp move (u/has-moved-up? move val))
                     move (u/set-move-currentHeight move val)]
                 move))]
    (vec vals)))

;; Get all the valid builds for a given move and board
(defn get-valid-builds
  [board move]
  (let [tokens (u/get-move-pieces move)
        piece (u/get-move-piecePos move)
        dec-pos (vec (map dec piece))
        xpos (get dec-pos 0)
        ypos (get dec-pos 1)
        player1 (get tokens 0)
        player2 (get tokens 1)
        vals (for [;; Map-indexed returns both the key and values for an item
                   ;;  which are then stored as x and row respectively
                   [x row] (map-indexed vector board)
                   [y val] (map-indexed vector row)
                   ;; Check that it is a valid build
                   :when (and (< val 4)
                              (within-one? x xpos)
                              (within-one? y ypos)
                              (or (not= x xpos) (not= y ypos))
                              ;; If any of the elements of players match [(inc x) (inc y)]
                              ;;  Then the build is invalid
                              (not (some #(= [(inc x) (inc y)] %) player1))
                              (not (some #(= [(inc x) (inc y)] %) player2)))]
               ;; if the build is valid, create the record and add it to the build list in move
               (let [bld (->Build [(inc x) (inc y)] (inc val))
                     move (u/append-build move bld)]
                 move))]
    (vec vals)))

;; Determines whether a given move has won
(defn has-won?
  [move card]
  (if (and (= (u/get-move-currentHeight move) 3)
           (u/get-move-hasMovedUp move))
    true
    (if (and (= card "Pan")
             (< 1 (- (u/get-move-startHeight move) (u/get-move-currentHeight move))))
      true
      false)))

;; checks whether two numbers are more than 1 apart
(defn within-one?
  [num1 num2]
  (not (> (Math/abs (- num1 num2)) 1)))

;; Checks whether the height of target is reachable from level with a given move
(defn movable-height?
  [level target move]
  (let [builds (u/get-move-builds move)
        can-reach (<= (- target level) 1)]
    ;; some logic for prometheus- don't allow moving up if we have already built
    (if (empty? builds)
      can-reach
      (and can-reach (<= target (u/get-move-currentHeight move))))))

;; adds two coordinates in an array of 2 digits together
(defn add-coords
  [coord]
  (+ (first coord) (second coord)))

;; Takes a string representing json and extracts out the json map
(defn read-json
  [json]
  (cheshire/parse-string json))

;; Takes a move, board, player position, and turn and creates the appropriate
;; map representing the move to convert to json 
(defn move-to-json
  [move board players turn]
  (let [turnmap (assoc {} "turn" (inc turn))
        pmap (assoc turnmap "players" (create-new-players players move))
        bmap (assoc pmap "spaces" (create-new-board board move))]
    bmap))
