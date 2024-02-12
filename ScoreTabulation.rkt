#lang racket


;Turns file input into a pair of teams.  A team is a list beginning with a name with the rest of the entries being games
;Precondition:   There are only 2 teams,  each team consists of precisely 15 games.  The input file must contain precisely 32 lines
;         with the name of the 1st team at line #1 and the name of the  2nd team at line #17
(define (make-team-records input-file)
  (let ([data (file->lines input-file)])
    (cons
     (cons (first data) (map line->game (rest(take data 16))))
     (cons (list-ref data 16) (map line->game (drop data 17))))))
   

;this exists for testing of line->game
(define example-game
  "Frosty Snoman X 7 2  4 5  8 /  3 6  X  X  5 / 9 / 1 8")

;A game is a pair with a player name as the first value and a list of frames as the second value  
(define (line->game line)
 (cons (extract-name-from-gamestring line) (extract-frames-from-gamestring line)))

;Takes in a string representing a game and returns a string containting the player name.
(define (extract-name-from-gamestring line)
 (string-join(append(take (string-split line " ") 2))))

;Takes in a string representing a game and returns a list of frames.  This function creates the list of tokens and passes that to bulid-frame-list
(define (extract-frames-from-gamestring line)
 (build-frame-list 
  (filter non-empty-string? (drop (string-split line " ") 2))))

;Begins constructing the next frame once it encounters an X or after creating a frame from 2 tokens
;The current-frame parameter is for the function's internal recursive logic.
; When begining a frame, this function is ran with the default paramater value (this should happen with all external calls)
;TODO: make function more resilient to unexpected data format
(define (build-frame-list remaining-tokens [current-frame null])
  (if (empty? remaining-tokens)
      current-frame  ;Base case, this will either return null or a 1 roll  frame if a spare granted an extra shot at the end
  (if (null? current-frame)
      (build-frame-list (rest remaining-tokens) (first remaining-tokens))
      (if (equal? current-frame "X")
          (cons "X" (build-frame-list remaining-tokens))
          (cons (append (list current-frame) (first remaining-tokens)) (build-frame-list (rest remaining-tokens)))))))



