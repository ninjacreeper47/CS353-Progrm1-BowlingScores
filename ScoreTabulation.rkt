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

(define example-game2
  "Chuck Babbage 8 / X 6 / 5 3 X X 4 5 8 / 2 7 9 / 9")

(define example-game3
  "Edgar Dikester X X 4 5 8 / X 3 / 7 2 X 9 / X X 8")

(define example-game4
  "Albert Perlis X X X 7 / X X 2 6 4 / 8 / X X X")

;A game is a pair with a player name as the first value and a list of frames as the second value  
(define (line->game line)
 (cons
  (extract-name-from-gamestring line)
  (extract-frames-from-gamestring line)))

;Takes in a string representing a game and returns a string containting the player name.
(define (extract-name-from-gamestring line)
 (string-join(append(take (string-split line " ") 2))))

;Takes in a string representing a game and returns a list of frames.  This function creates the list of tokens and passes that to bulid-frame-list
;The list of tokens does not include whitespace and all digits are numeric types instead of strings
(define (extract-frames-from-gamestring line)
  (build-frame-list
 ;  (fix-those-annoying-games-with-incomplete-frames
   (map string->number-or-string
   (filter non-empty-string? (drop (string-split line " ") 2)))))

;Begins constructing the next frame once it encounters an X or after creating a frame from 2 tokens
;The current-frame parameter is for the function's internal recursive logic.
; When begining a frame, this function is ran with the default paramater value (this should happen with all external calls)
;TODO: make function more resilient to unexpected data format
(define (build-frame-list remaining-tokens [current-frame null])
  (if (empty? remaining-tokens) ;Base case, this will either return null or a 1 roll frame padded to be 2 numbers with an extra 0 roll
      remaining-tokens
      (if (null? current-frame)
          (build-frame-list (rest remaining-tokens) (first remaining-tokens))
          (if (equal? current-frame "X")
              (cons "X" (build-frame-list remaining-tokens))
              (cons (append (list current-frame) (first remaining-tokens)) (build-frame-list (rest remaining-tokens)))))))


;Frames are scores in reverse order, so each one can supply next roll values to it's previous frame
(define (score-game game)
  (let ([frames (reverse (rest game))])
    (foldl score-frame '(0 0 0) frames)))


;Outputs a list consisting of the score, the number of pins knocked down with the first roll, and the number of pins knocked down with the 2nd roll
;This function is designed around the next/2nd-next roll parameters being already calculated
(define (score-frame frame [score-and-next-rolls-list '(0 0 0)])
  (let ([current-score (first score-and-next-rolls-list)] [next-roll (second score-and-next-rolls-list)] [2nd-next-roll (third score-and-next-rolls-list)])
  (cond
    [(equal? frame "X") (list (+ current-score 10 next-roll 2nd-next-roll) 10  next-roll)] ;frame doesn't have 2 rolls so next-roll is carried over as the 2nd roll
    [(equal? (cdr frame) "/") (list (+ current-score 10 next-roll) (car frame) (- 10 (car frame)))]
    [else (list (+ current-score (car frame) (cdr frame)) (car frame) (cdr frame))])))


;Converts a string to a number if the string is numeric, otherwise returns the string back. Returns null if given null
(define (string->number-or-string string)
  (if (null? string)
      null
      (if (string->number string)
          (string->number string)
          string)))

(define (report input-file)
 (let ([team-record (make-team-records input-file)])
   (team-scores team-record)))

;Team 1 works, team 2 has input that my program isn't properly handling. The print statements are for viewing team 1 results before program crash :)
(define (team-scores team-record)
   (printf (first(first team-record))) ;team 1 name
    (printf(number->string(foldl + 0  (map first (map score-game  (rest(first team-record))))))) ;team 1 score
   (cons
    (first(rest team-record)) ;team 2 name
    (foldl + 0  ( map first (map score-game  (rest(rest team-record))))))) ;team 2 score
   