#lang racket/gui

(require "collide.rkt" "languages.rkt" racket/block)

; Stuff
(define (r1 c) (add1 (random (floor c))))
(define (r0 c) (let ([xc (abs (inexact->exact (ceiling c)))]) (add1 (- (sub1 (/ xc 2)) (random xc)))))
(define (rcol) (make-object color% (random 255) (random 255) (random 255)))
(define (subsquare a b)
  (expt (- a b) 2))
(define (distance-between x1 y1 x2 y2)
  (sqrt (+ (subsquare x2 x1) (subsquare y2 y1))))

(define (invert-colour c)
  (make-object color% (- 255 (send c red)) (- 255 (send c green)) (- 255 (send c blue))))

(define (colsafe newval)
  (cond [(> newval 255) 255]
        [(< newval 0) 0]
        [else newval]))
(define (rc+ rgbval)
  (colsafe (+ rgbval (r0 16))))
(define (rcol+ c)
  (make-object color% (rc+ (send c red)) (rc+ (send c green)) (rc+ (send c blue))))
(define col-
  (let ([subsum 4])
    (λ (c)
      (make-object color% 
        (colsafe (- (send c red) subsum)) 
        (colsafe (- (send c green) subsum)) 
        (colsafe (- (send c blue) subsum))))))

(define (black? col)
  (and (zero? (send col red)) (zero? (send col green)) (zero? (send col blue))))
(define (brightness col)
  (floor (/ (+ (send col red) (send col green) (send col blue)) 3)))

(define field-scale 20)

; Plants
(struct Plant (colour [curcol #:mutable] x y) #:transparent)

(define plantlife (make-hash))

(define (Plant-point p)
  (list (Plant-x p) (Plant-y p)))

(define (new-plant! x y [clr (rcol)]) 
  (hash-set! plantlife (list x y) (Plant clr clr x y)))

(define adjacencies
  (let ([adj-list '((-1 -1) (0 -1) (1 -1) 
                            (-1 0) (1 0)
                            (-1 1) (0 1) (1 1))])
    (λ (x y w h)
      (filter
       (λ (pt) (and (> (first pt) 0) (< (first pt) w)
                    (> (second pt) 0) (< (second pt) h)))
       (map (λ (adj) (list (+ x (first adj)) (+ y (second adj))))
            adj-list)))))

(define (infertile? p)
  (> (random 256)
     (brightness (Plant-curcol p))))

(define (step-plants! dc) 
  (let-values ([(w h) (send dc get-size)]) 
    (for-each
     (λ (p)
       (set-Plant-curcol! p (col- (Plant-curcol p)))
       (when (black? (Plant-curcol p))
         (hash-remove! plantlife (Plant-point p)))
       (for-each
        (λ (adj) (unless (or (infertile? p) (hash-has-key? plantlife adj))
                   (new-plant! (first adj) (second adj) (rcol+ (Plant-colour p)))))
        (adjacencies (Plant-x p) (Plant-y p) (floor (/ w field-scale)) (floor (/ h field-scale)))))
     (hash-values plantlife))))

(define (paint-plants! dc)
  (send dc set-smoothing 'smoothed)
  (send dc set-pen "black" 1 'transparent)
  (send dc set-brush "black" 'solid)
  (let-values ([(w h) (send dc get-size)]) (send dc draw-rectangle 0 0 w h))
  (for-each (λ (p)
              (send dc set-pen (Plant-curcol p) field-scale 'solid)
              (send dc set-brush (Plant-curcol p) 'solid)
              (send dc draw-line (* field-scale (Plant-x p)) (* field-scale (Plant-y p)) 
                    (* field-scale (Plant-x p)) (* field-scale (Plant-y p))))
            (hash-values plantlife)))

; Structure
(define frame (new frame%
                   [label "Plants"]
                   [width 800] [height 600]))

(define panel1 (new vertical-panel% [parent frame]))

(define critter-canvas%
  (class canvas%
    (define/override (on-event event)
      (when (and (is-a? event mouse-event%) (equal? (send event get-event-type) 'left-up))
        (new-plant! (floor (/ (send event get-x) field-scale)) (floor (/ (send event get-y) field-scale)))))
    (super-new)))
(define cv (new critter-canvas% [parent panel1] 
                [paint-callback (λ (canvas dc) (paint-plants! dc))]))
(send frame show #t)
(define tim (new timer% 
                 [notify-callback (λ () 
                                    (let ([dc (send cv get-dc)])
                                      (send cv suspend-flush)
                                      (step-plants! dc) 
                                      (paint-plants! dc)
                                      (send cv resume-flush))
                                    (unless (send frame is-shown?) (send tim stop)))]
                 [interval 100]))
