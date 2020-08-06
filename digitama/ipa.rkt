#lang racket/base

(provide (all-defined-out))

(require racket/list)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-vowel-heights '(Close Near-Close Close-Mid Mid Open-Mid Near-Open Open))
(define ipa-vowel-backnesses '(Front Near-Front _ _ Central _ _ Near-Back Back))
(define ipa-vowel-simplified-backnesses '(Front Central Back))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define ipa-vowel-position
  (lambda [width height vowel-backness vowel-height [dx 0] [dy 0]]
    (define-values (backness-total height-total) (values (length ipa-vowel-backnesses) (length ipa-vowel-heights)))
    (define-values (backness-scale height-scale) (values (/ 1 (sub1 backness-total)) (/ 1 (sub1 height-total))))
    (define backness-index (or (index-of ipa-vowel-backnesses vowel-backness eq?) backness-total))
    (define height-index (- height-total (or (index-of ipa-vowel-heights vowel-height eq?) height-total)))
    (define front-open-x (* width 1/2))
    (define line-width (+ (- width front-open-x) (* front-open-x height-scale height-index)))
    
    (values (+ (- width line-width) (* line-width backness-scale backness-index) dx)
            (+ (* height height-scale height-index) dy))))

(define ipa-vowel-positions
  (lambda [width height vowel-height [dx 0] [dy 0]]
    (for/fold ([xs null]
               [y 0])
              ([backness (in-list (reverse ipa-vowel-simplified-backnesses))])
      (define-values (x y) (ipa-vowel-position width height backness vowel-height dx dy))
      (values (cons x xs) y))))
