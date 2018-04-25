#lang racket/gui
(require "src/soko_ui.rkt")

(define (main)
  (define file (get-file "Please choose a Sokoban level file" #f (current-directory)))
  (main-draw file))

(main)