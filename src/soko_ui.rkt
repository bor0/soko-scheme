#lang racket
(require racket/draw)
(require racket/gui)
(require racket/math)
(require "game.rkt")
(require "level.rkt")

(define asset-width 30)
(define asset-height 30)
(define soko-map null)

(define (main-draw file)
  (if file
      (begin
        (set! soko-map (read-level file))
        (run))
      #f))

(define soko-bitmaps
  (list
   (list 'LEVEL_EMPTY (read-bitmap "./gfx/empty.png"))
   (list 'LEVEL_WALL (read-bitmap "./gfx/wall.png"))
   (list 'LEVEL_TERRAIN (read-bitmap "./gfx/terrain.png"))
   (list 'LEVEL_BOX (read-bitmap "./gfx/box.png"))
   (list 'LEVEL_BEACON (read-bitmap "./gfx/beacon.png"))
   (list 'LEVEL_BOX_BEACON (read-bitmap "./gfx/b_beacon.png"))
   (list 'LEVEL_SOKOBAN (read-bitmap "./gfx/sokoban.png"))
   (list 'LEVEL_SOKOBAN_BEACON (read-bitmap "./gfx/sokoban.png"))))

(define (get-soko-bitmap asset)
  (let ([bitmap (findf (lambda (x) (eq? (car x) asset)) soko-bitmaps)])
    (if bitmap (cadr bitmap) (cadar soko-bitmaps))))

(define (draw-map canvas dc)
  (if (check-win? soko-map)
      (send dc draw-text "You Won!" 0 0)
      (for* ([i (length (car soko-map))]
             [j (length soko-map)])
        (send dc draw-bitmap (get-soko-bitmap (get-soko-map-value soko-map (posn i j))) (* i asset-width) (* j asset-height)))))

; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (let ([keycode (send event get-key-code)])
        (cond ((eq? keycode #\w) (set! soko-map (play soko-map 'UP)))
              ((eq? keycode #\a) (set! soko-map (play soko-map 'LEFT)))
              ((eq? keycode #\s) (set! soko-map (play soko-map 'DOWN)))
              ((eq? keycode #\d) (set! soko-map (play soko-map 'RIGHT))))
        (send this refresh-now)))
    ; Call the superclass init, passing on all init args
    (super-new)))

(define (run)
  ; Make a frame by instantiating the frame% class
  (letrec ([frame (new frame% [label "Sokoban v1.0 by Boro Sitnikovski"]
                       [width (* asset-width (length (car soko-map)))] [height (+ 50 (* asset-height (length soko-map)))])])
           ; Show the frame by calling its show method
           (new my-canvas% [parent frame] [paint-callback draw-map])
           (send frame show #t)))

(provide main-draw)
