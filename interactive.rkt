#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require graphics/graphics)

;; TODO - try implementing Wallingford, Elm, and FrTime demos.
;; TODO - explore the relational aspect much more.

;;; window size
(define horiz 300)
(define vert 300)

(define horiz-center (quotient horiz 2))
(define vert-center (quotient vert 2))


(define (current-milliseconds)
  (inexact->exact (floor (current-inexact-milliseconds))))

;;; Display all the factors of n, where n is the x position of the cursor, mod 50.
(define (mult-by-x-pos)
  (open-graphics)
  (let ((w (open-viewport "mult-by-x-pos" horiz vert)))
    (dynamic-wind
        void
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
        (begin
          (close-viewport w)
          (close-graphics)))))

;;; Draw lines, whose x/y coordinates reflect all the factors of n,
;;; where n is the x position of the cursor, mod 50.
(define (draw-line-mult-by-x-pos)
  (open-graphics)
  (let ((w (open-viewport "draw-line-mult-by-x-pos" horiz vert)))
    (dynamic-wind
        void
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
        (begin
          (close-viewport w)
          (close-graphics)))))


;;; Draw lines, whose x/y coordinates reflect all the factors of n,
;;; where n is the current time in seconds, mod 50.
;;; Uses busy-waiting to wait for the second to increment.
(define (draw-line-mult-by-time)
  (open-graphics)
  (let ((w (open-viewport "draw-line-mult-by-time" horiz vert)))
    (dynamic-wind
        void
        (let loop ((old-time (modulo (current-seconds) 50)))
          (let ((time (modulo (current-seconds) 50)))
            (if (= time old-time)
                (loop time)
                (let ((ans (run* (a b) (*o a b (build-num time)))))
                                        ; (displayln ans)
                  (let ((factor-ls  (map (lambda (a) (map unbuild-num a)) ans)))
                    (displayln factor-ls)
                    ((clear-viewport w))
                    ((draw-string w) (make-posn horiz-center vert-center) (number->string time))
                    (for-each
                      (lambda (x/y)
                        ((draw-line w)
                         (make-posn 0 0)
                         (make-posn (+ 0 (* (car x/y) 10))
                                    (+ 0 (* (cadr x/y) 10)))))
                      factor-ls)
                    (loop time))))))
        (begin
          (close-viewport w)
          (close-graphics)))))


;;; Draw lines, whose x/y coordinates reflect all the factors of n,
;;; where n is the current time in seconds, mod 50.
;;; Uses a timed callback to wait for the second to increment.
(define (draw-line-mult-by-time-callback)
  (open-graphics)
  (let ((w (open-viewport "draw-line-mult-by-time-callback" horiz vert)))
    (dynamic-wind
        void
        ((set-on-tick-event w)	 	 	 	 
         1000
         (lambda args
           (let ((time (modulo (current-seconds) 50)))
             (let ((ans (run* (a b) (*o a b (build-num time)))))
                                        ; (displayln ans)
               (let ((factor-ls  (map (lambda (a) (map unbuild-num a)) ans)))
                 (displayln factor-ls)
                 ((clear-viewport w))
                 ((draw-string w) (make-posn horiz-center vert-center) (number->string time))
                 (for-each
                   (lambda (x/y)
                     ((draw-line w)
                      (make-posn 0 0)
                      (make-posn (+ 0 (* (car x/y) 10))
                                 (+ 0 (* (cadr x/y) 10)))))
                   factor-ls))))))
        ;; don't close the viewpoint yet!  the callback won't be called for another second!
        void)))
