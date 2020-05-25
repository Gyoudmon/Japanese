#lang racket

(provide (all-defined-out))

(require scribble/core)
(require scribble/manual)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ruby
  (lambda [base ruy]
    (make-multiarg-element "ruby" (list base ruy))))

(define define-term
  (lambda [en kenji hiragana #:key [key #false]]
    (list (deftech en #:key key)
          ~
          "「" (ruby kenji hiragana) "」")))
