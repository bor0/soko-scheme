#lang racket
(define-struct posn (x y))

; Bounds checker
(define (bad-bounds? soko-map pos) (or (>= (posn-x pos) (length (car soko-map)))
                                       (>= (posn-y pos) (length soko-map))
                                       (< (posn-x pos) 0)
                                       (< (posn-y pos) 0)))

; Get a value from the map given position
(define (get-soko-map-value soko-map pos)
  (if (bad-bounds? soko-map pos)
      #f
      (list-ref (list-ref soko-map (posn-y pos)) (posn-x pos))))

(define (set-matrix xss i j x)
  (cond
    [(empty? xss) '()]
    [(= j 0)      (cons (list-set (car xss) i x)
                        (cdr xss))]
    [else         (cons (car xss)
                        (set-matrix (cdr xss) i (- j 1) x))]))

; Return a new updated map with new value at position
(define (set-soko-map-value soko-map pos val)
  (if (bad-bounds? soko-map pos)
      soko-map
      (set-matrix soko-map (posn-x pos) (posn-y pos) val)))

; Find the position of LEVEL_SOKOBAN or LEVEL_SOKOBAN_BEACON
(define (get-soko-position soko-map)
  (car (for*/list ([i (length (car soko-map))]
                   [j (length soko-map)]
                   #:when (member (get-soko-map-value soko-map (make-posn i j)) '(LEVEL_SOKOBAN LEVEL_SOKOBAN_BEACON)))
         (make-posn i j))))

; Calculate new position given a position and direction
(define (get-new-position soko-map soko-position direction)
  (cond ((eq? direction 'UP) (make-posn (posn-x soko-position)
                                        (- (posn-y soko-position) 1)))
        ((eq? direction 'DOWN) (make-posn (posn-x soko-position)
                                          (+ 1 (posn-y soko-position))))
        ((eq? direction 'LEFT) (make-posn (- (posn-x soko-position) 1)
                                          (posn-y soko-position)))
        ((eq? direction 'RIGHT) (make-posn (+ 1 (posn-x soko-position))
                                           (posn-y soko-position)))))

; Move box from a position to a new direction, maintaining beacon state
(define (update-box soko-map box-pos direction)
  (letrec ([box-type (get-soko-map-value soko-map box-pos)]
           [new-box-pos (get-new-position soko-map box-pos direction)]
           [new-box-type (get-soko-map-value soko-map new-box-pos)])
    (cond ((eq? new-box-type 'LEVEL_BEACON)
           (set-soko-map-value (set-soko-map-value soko-map new-box-pos 'LEVEL_BOX_BEACON)
                               box-pos
                               (if (eq? box-type 'LEVEL_BOX_BEACON)
                                   'LEVEL_BEACON
                                   'LEVEL_TERRAIN)))
          ((eq? new-box-type 'LEVEL_TERRAIN)
           (set-soko-map-value (set-soko-map-value soko-map new-box-pos 'LEVEL_BOX)
                               box-pos
                               (if (eq? box-type 'LEVEL_BOX_BEACON)
                                   'LEVEL_BEACON
                                   'LEVEL_TERRAIN)))
          (else #f))))

; Move soko from a position to a new direction, maintaining beacon state
(define (update-soko soko-map soko-pos direction)
  (letrec ([soko-type (get-soko-map-value soko-map soko-pos)]
           [new-soko-pos (get-new-position soko-map soko-pos direction)]
           [new-soko-type (get-soko-map-value soko-map new-soko-pos)]
           [removed-soko-map (set-soko-map-value soko-map soko-pos (if (eq? soko-type 'LEVEL_SOKOBAN_BEACON) 'LEVEL_BEACON 'LEVEL_TERRAIN))])
    (cond ((bad-bounds? soko-map new-soko-pos) soko-map)
          ((eq? new-soko-type 'LEVEL_BEACON)
           (set-soko-map-value removed-soko-map new-soko-pos 'LEVEL_SOKOBAN_BEACON))
          ((eq? new-soko-type 'LEVEL_TERRAIN)
           (set-soko-map-value removed-soko-map new-soko-pos 'LEVEL_SOKOBAN))
          ((eq? new-soko-type 'LEVEL_BOX)
           (let ([new-box-map (update-box removed-soko-map new-soko-pos direction)])
             (if new-box-map (set-soko-map-value new-box-map new-soko-pos 'LEVEL_SOKOBAN)
                 soko-map)))
          ((eq? new-soko-type 'LEVEL_BOX_BEACON)
           (let ([new-box-map (update-box removed-soko-map new-soko-pos direction)])
             (if new-box-map (set-soko-map-value new-box-map new-soko-pos 'LEVEL_SOKOBAN_BEACON)
                 soko-map)))
          (else soko-map))))

(define (play soko-map direction)
  (update-soko soko-map (get-soko-position soko-map) direction))

; It's a win only if there are no beacons
(define (check-win? soko-map)
  (and
   (not (member 'LEVEL_BEACON (flatten soko-map)))
   (not (member 'LEVEL_SOKOBAN_BEACON (flatten soko-map)))))

(provide play posn posn-x posn-y set-soko-map-value get-soko-position get-soko-map-value check-win?)
