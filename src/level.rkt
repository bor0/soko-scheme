#lang racket
(require "game.rkt")

(define (read-level file)
  (convert-to-soko-map
   (map (lambda (l)
          (map string->number (string-split l " ")))
        (file->lines file))))

(provide read-level)
