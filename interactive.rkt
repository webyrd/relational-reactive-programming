#lang racket

(require "mk.rkt")
(require "numbers.rkt")
(require graphics/graphics)

;; TODO - try implementing Wallingford, Elm, and FrTime demos.
;; TODO - explore the relational aspect much more.

(define pi 3.14159)
(define two-pi (* pi 2))

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
                  (let ((ans (run* (a b) (*o a b (build-num x)))))
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
        ;; Don't close the viewpoint yet!  The callback won't be called for another second!
        ;; What is the right way to clean up here?
        void)))


;;; Use the x position of the mouse, mod 6, to determine which answer
;;; from (run* (l s) (appendo l s '(a b c d e))) to display.
(define (scrub-run-n-appendo)
  (open-graphics)
  (let ((w (open-viewport "scrub-run-n-appendo" horiz vert)))
    (dynamic-wind
        void
        (let loop ((p (query-mouse-posn w))
                   (old-x 0))
          (if p
              (let ((x (modulo (posn-x p) 6)))
                (when (not (= x old-x))
                  (let ((ans (run* (l s) (appendo l s '(a b c d e)))))
                    ((clear-viewport w))
                    (let ((l/s (list-ref ans x)))
                      ((draw-string w) (make-posn 10 100) (format "~s" (car l/s)))
                      ((draw-string w) (make-posn 100 100) (format "~s" (cadr l/s)))))
                  (viewport-flush-input w))
                (loop (query-mouse-posn w) x))
              (loop (query-mouse-posn w) old-x)))
        (begin
          (close-viewport w)
          (close-graphics)))))


;;; Draw a line representing the seconds hand of a clock (current-seconds mod 60).
;;; Doesn't use miniKanren.
(define (simple-clock-by-time)
  (open-graphics)
  (let ((w (open-viewport "draw-line-mult-by-time" horiz vert)))
    (dynamic-wind
        void
        (let loop ((old-time (modulo (current-seconds) 60)))
          (let ((time (modulo (current-seconds) 60)))
            (if (= time old-time)
                (loop time)
                (let ((angle (- (* (/ two-pi 60) time) (/ two-pi 4))))
                  (let ((length 300.0))
                    (let ((x (* (cos angle) length))
                          (y (* (sin angle) length)))
                      (displayln time)
                      (displayln x)
                      (displayln y)
                      ((clear-viewport w))
                      ((draw-string w) (make-posn 10 10) (number->string time))
                      ((draw-line w)
                       (make-posn horiz-center vert-center)
                       (make-posn (+ horiz-center x)
                                  (+ vert-center y)))
                      (loop time)))))))
        (begin
          (close-viewport w)
          (close-graphics)))))
