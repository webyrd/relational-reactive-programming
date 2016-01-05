#lang racket

;;; Interactive temerature conversion, between F and C.
;;; Inspired by Jonathan Edwards's Two-Way Dataflow example in Subtext: https://vimeo.com/106073134
;;; Thanks to Matt Hammer for the suggestion!

;; Caveats:
;; * Numbers are "Oleg numerals" (little-endian binary lists)
;; * No negative values allowed!
;; * The goals in the run expressions are reordered to prevent divergence.
;; An alternative approach would be to use a timeout.

(require "mk.rkt")
(require "numbers.rkt")
(require racket/gui/base)

(define (temp-conversion)
  (let ((frame (new frame% (label "frame"))))
    (let ((display-F (new message% (parent frame)
                          (label "display-F")
                          (auto-resize #t)))
          (display-C (new message% (parent frame)
                          (label "display-C")
                          (auto-resize #t))))
      
      (letrec ((txt-F (new text-field%
                           (label "F")
                           (parent frame)
                           (init-value "?")
                           (callback (lambda (button event)
                                       (let ((F-str (send button get-value)))
                                         (let ((sp (open-input-string F-str)))
                                           (let ((F (read sp)))
                                             (let ((ans (run 1 (C)
                                                          (fresh (C*9 C*9/5 rem)
                                                            (pluso C*9/5 (build-num 32) F)
                                                            (/o C*9 (build-num 5) C*9/5 rem)
                                                            (*o C (build-num 9) C*9)))))
                                               (if (null? ans)
                                                   (begin
                                                     (send display-C set-label "-")
                                                     (send display-F set-label "-")
                                                     (send txt-C set-value "?"))
                                                   (begin
                                                     (send display-F set-label (format "~s" F))
                                                     (send display-C set-label (format "~s" (car ans)))
                                                     (send txt-C set-value "?")))))))))))
               (txt-C (new text-field%
                           (label "C")
                           (parent frame)
                           (init-value "?")
                           (callback (lambda (button event)
                                       (let ((C-str (send button get-value)))
                                         (let ((sp (open-input-string C-str)))
                                           (display "reading C string...")
                                           (newline)
                                           (let ((C (read sp)))
                                             (display "C: ")
                                             (display C)
                                             (newline)
                                             (let ((ans (run 1 (F)
                                                          (fresh (C*9 C*9/5 rem)
                                                            (*o C (build-num 9) C*9)
                                                            (/o C*9 (build-num 5) C*9/5 rem)
                                                            (pluso C*9/5 (build-num 32) F)))))
                                               (display "ans: ")
                                               (display ans)
                                               (newline)
                                               (if (null? ans)
                                                   (begin
                                                     (send display-C set-label "-")
                                                     (send display-F set-label "-")
                                                     (send txt-F set-value "?"))
                                                   (begin
                                                     (send display-C set-label (format "~s" C))
                                                     (send display-F set-label (format "~s" (car ans)))
                                                     (send txt-F set-value "?"))))))))))))
(send frame show #t)))))
