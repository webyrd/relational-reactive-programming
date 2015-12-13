#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require graphics/graphics)

;; TODO - try implementing Wallingford, Elm, and FrTime demos.
;; TODO - explore the relational aspect much more.

;;; display all the factors of n, where n is the x position of the cursor, mod 50
(define (mult-by-x-pos)

  (open-graphics)
  (define w (open-viewport "practice" 300 300))
  
  (let loop ((p (query-mouse-posn w))
             (old-x 0))
    (if p
        (let ((x (modulo (posn-x p) 50)))
          (when (not (= x old-x))
            (displayln x)
            (let ((ans (run* (a b) (*o a b (build-num x)))))
              (displayln ans)
              (displayln (map (lambda (a) (map unbuild-num a)) ans)))
            (viewport-flush-input w))
          (loop (query-mouse-posn w) x))
        (loop (query-mouse-posn w) old-x)))

  (close-viewport w)
  (close-graphics)
  )

;;; draw lines, whose x/y coordinates reflect all the factors of n,
;;; where n is the x position of the cursor, mod 50
(define (draw-line-mult-by-x-pos)

  (define horiz 300)
  (define vert 300)

  (define horiz-center (quotient horiz 2))
  (define vert-center (quotient vert 2))
  
  (open-graphics)
  (define w (open-viewport "practice" horiz vert))
    
  (let loop ((p (query-mouse-posn w))
             (old-x 0))
    (if p
        (let ((x (modulo (posn-x p) 50)))
          (when (not (= x old-x))
            ; (displayln x)
            (let ((ans (run* (a b) (*o a b (build-num x)))))
              ; (displayln ans)
              (let ((factor-ls  (map (lambda (a) (map unbuild-num a)) ans)))
                (displayln factor-ls)
                ((clear-viewport w))
                (for-each
                 (lambda (x/y)
                   ((draw-line w)
                    (make-posn 0 0)
                    (make-posn (+ 0 (* (car x/y) 4))
                               (+ 0 (* (cadr x/y) 4)))))
                 factor-ls)
                ))
            (viewport-flush-input w))
          (loop (query-mouse-posn w) x))
        (loop (query-mouse-posn w) old-x)))

  (close-viewport w)
  (close-graphics)
  )
