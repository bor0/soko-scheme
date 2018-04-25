#lang racket
(require "game.rkt")

(define soko-enums
  '((0 LEVEL_EMPTY)
    (1 LEVEL_WALL)
    (2 LEVEL_TERRAIN)
    (3 LEVEL_BOX)
    (4 LEVEL_BEACON)
    (5 LEVEL_BOX_BEACON)
    (6 LEVEL_SOKOBAN)
    (7 LEVEL_SOKOBAN_BEACON)))

; Read level from a file into a list of lists
(define (read-level file)
  (convert-to-soko-map
   (map (lambda (l)
          (map string->number (string-split l " ")))
        (file->lines file))))

; Define map for matrices
(define (map-soko-map f soko-map)
  (map (lambda (x) (map f x)) soko-map))

; Convert a matrix to a soko map using soko-enums
(define (convert-to-soko-map matrix)
  (map-soko-map
   (lambda (x)
     (let ([found-val (findf (lambda (y) (= (car y) x)) soko-enums)])
       (if (pair? found-val) (cadr found-val) 'LEVEL_TERRAIN)))
   matrix))

; Convert a soko-map to a matrix using soko-enums
(define (convert-to-matrix soko-map)
  (map-soko-map
   (lambda (x)
     (car (findf (lambda (y) (eq? (cadr y) x)) soko-enums))) soko-map))

(provide read-level convert-to-soko-map convert-to-matrix)
