#lang racket
(require racket/draw)
(require racket/gui)
(require racket/math)
(require "game.rkt")
(require "level.rkt")

(define asset-width 30)
(define asset-height 30)
(define soko-map null)
(define moves 0)

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

; Entry point for the game
(define (main-draw file)
  (if file
      (begin
        (set! soko-map (read-level file))
        (run))
      #f))

; Get a soko bitmap for specific asset (e.g. 'LEVEL_SOKOBAN)
(define (get-soko-bitmap asset)
  (let ([bitmap (findf (lambda (x) (eq? (car x) asset)) soko-bitmaps)])
    (if bitmap (cadr bitmap) (cadar soko-bitmaps))))

; Draw the Soko level to the dc, by using bitmap for each asset
(define (draw-map canvas dc)
  (if (check-win? soko-map)
      (send dc draw-text (format "You Won in ~a moves!" moves) 0 0)
      (for* ([i (length (car soko-map))]
             [j (length soko-map)])
        (send dc draw-bitmap (get-soko-bitmap (get-soko-map-value soko-map (posn i j))) (* i asset-width) (* j asset-height)))))

; Teleport functionality where we can move the position of the Soko to any terrain
(define (teleport x y)
  (letrec ([soko-pos (get-soko-position soko-map)]
        [soko-type (get-soko-map-value soko-map soko-pos)]
        [newpos (posn (floor (/ x asset-width)) (floor (/ y asset-height)))]
        [removed-soko-map (set-soko-map-value soko-map soko-pos
                                              (if (eq? soko-type 'LEVEL_SOKOBAN_BEACON) 'LEVEL_BEACON 'LEVEL_TERRAIN))])
    (if (member (get-soko-map-value soko-map newpos) '(LEVEL_TERRAIN))
        (set! soko-map (set-soko-map-value removed-soko-map newpos 'LEVEL_SOKOBAN))
        soko-map)))

; Music thread keeps running itself
(define (music-thread)
  (play-sound "./music/untzuntz.mp3" #f)
  (music-thread))

; Derive a new frame class to handle events
(define my-frame%
 (class frame%
   (define music-thread-id (thread music-thread))
   ; Close music thread on exit
   (define/override (on-exit)
     (kill-thread music-thread-id))
   (super-new)))

(define (key? sort keycode)
  (or (eq? sort keycode)
      (case sort
        ((up) (eq? keycode #\w))
        ((left) (eq? keycode #\a))
        ((down) (eq? keycode #\s))
        ((right) (eq? keycode #\d))
        (else #f))))

; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ; Define overriding method to handle mouse events
    (define/override (on-event event)
      (let ([keycode (send event get-shift-down)])
        (if keycode (teleport (send event get-x)
                              (send event get-y)) #f)))
    ; Define overriding method to handle keyboard events
    (define/override (on-char event)
      (let ([keycode (send event get-key-code)])
        (set! moves (+ moves 1))
        (cond ((key? 'up keycode) (set! soko-map (play soko-map 'UP)))
              ((key? 'left keycode) (set! soko-map (play soko-map 'LEFT)))
              ((key? 'down keycode) (set! soko-map (play soko-map 'DOWN)))
              ((key? 'right keycode) (set! soko-map (play soko-map 'RIGHT)))
              (else (set! moves (- moves 1))))
        (send this refresh-now)))
    ; Call the superclass init, passing on all init args
    (super-new)))

(define (run)
  ; Make a frame by instantiating the frame% class
  (letrec ([frame (new my-frame% [label "Sokoban v1.0 by Boro Sitnikovski"]
                       [width (* asset-width (length (car soko-map)))] [height (+ 50 (* asset-height (length soko-map)))])])
           ; Show the frame by calling its show method
           (new my-canvas% [parent frame] [paint-callback draw-map])
           (send frame show #t)))

(provide main-draw)
