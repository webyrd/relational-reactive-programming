#lang racket

;;; Interactive examples, using the full Racket GUI toolkit.

(require "mk.rkt")
(require "numbers.rkt")
(require racket/gui/base)

(define pi 3.14159)
(define two-pi (* pi 2))

;;; window size
(define horiz 300)
(define vert 300)

(define horiz-center (quotient horiz 2))
(define vert-center (quotient vert 2))


(define (current-milliseconds)
  (inexact->exact (floor (current-inexact-milliseconds))))


;;; Use the slider to determine which answer
;;; from (run* (l s) (appendo l s '(a b c d e))) to display.
(define (slider-run-n-appendo)
  (let ((frame (new frame% [label "slider-run-n-appendo"])))
    (let ((x-msg (new message% [parent frame]
                      [label "answer for l"]))
          (y-msg (new message% [parent frame]
                      [label "answer for s"])))
      (let ((scrubber (new slider%
                           (label "run n")
                           (parent frame)
                           (min-value 1)
                           (max-value 6)
                           (init-value 1)
                           (callback (lambda (button event)
                                       (let ((n (sub1 (send button get-value))))
                                         (let ((ans (run* (l s) (appendo l s '(a b c d e)))))
                                           (let ((l/s (list-ref ans n)))
                                             (send x-msg set-label (format "~s" (car l/s)))
                                             (send y-msg set-label (format "~s" (cadr l/s)))))))))))
        (send frame show #t)))))
