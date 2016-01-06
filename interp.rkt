#lang racket

;;; Interactive Schemely interpreter

;;; Issues:
;;;
;;; * Running *backwards* easily causes divergence, since the interpreter code gives preference to a known input expression. Perhaps a timeout, or asynchronous execution in the background, is in order.
;;; * When running backwards, there may be more than one corresponding input expression, or even infinitely many.  Perhaps add a slider, or have a larger display area, for the input expressions.
;;; * Currently no support for variables within the expression/value.
;;; * Currently can't specify both the expression and value to check for consistency, or partially specify both (for specifiying quines, for example).

(require "mk.rkt")
(require "interp-uber.rkt")
(require racket/gui/base)

(define (interpreter)
  (let ((frame (new frame% (label "frame"))))
    (let ((display-expr (new message% (parent frame)
                             (label "display-expr")
                             (auto-resize #t)))
          (display-value (new message% (parent frame)
                              (label "display-value")
                              (auto-resize #t))))      
      (letrec ((txt-expr (new text-field%
                              (label "expr")
                              (parent frame)
                              (init-value "?")
                              (callback (lambda (button event)
                                          (with-handlers ([exn:fail?
                                                           (lambda (exn)
                                                             (send display-value set-label "-")
                                                             (send display-expr set-label "-")
                                                             (send txt-value set-value "?"))])
                                            (let ((expr-str (send button get-value)))
                                              (let ((sp (open-input-string expr-str)))
                                                (let ((expr (read sp)))
                                                  (let ((ans (run 1 (value) (evalo expr value))))
                                                    (if (null? ans)
                                                        (begin
                                                          (send display-value set-label "-")
                                                          (send display-expr set-label "-")
                                                          (send txt-value set-value "?"))
                                                        (begin
                                                          (send display-expr set-label (format "~s" expr-str))
                                                          (send display-value set-label (format "~s" (car ans)))
                                                          (send txt-value set-value "?"))))))))))))
               (txt-value (new text-field%
                               (label "value")
                               (parent frame)
                               (init-value "?")
                               (callback (lambda (button event)
                                           (with-handlers ([exn:fail?
                                                            (lambda (exn)
                                                              (send display-value set-label "-")
                                                              (send display-expr set-label "-")
                                                              (send txt-expr set-value "?"))])
                                             (let ((value-str (send button get-value)))
                                               (let ((sp (open-input-string value-str)))
                                                 (display "reading value string...")
                                                 (newline)
                                                 (let ((value (read sp)))
                                                   (display "value: ")
                                                   (display value)
                                                   (newline)
                                                   (let ((ans (run 1 (expr) (evalo expr value))))
                                                     (display "ans: ")
                                                     (display ans)
                                                     (newline)
                                                     (if (null? ans)
                                                         (begin
                                                           (send display-expr set-label "-")
                                                           (send display-value set-label "-")
                                                           (send txt-expr set-value "?"))
                                                         (begin
                                                           (send display-value set-label (format "~s" value))
                                                           (send display-expr set-label (format "~s" (car ans)))
                                                           (send txt-expr set-value "?")))))))))))))
        (send frame show #t)))))
