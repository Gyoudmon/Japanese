#lang at-exp racket

(provide (all-defined-out))

(require "literacy.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define PoS @ja-tech[#:key "part of speech"]{parts of speech})

(define NP @ja-tech[#:key "NP"]{noun phrase})
(define VP @ja-tech[#:key "VP"]{verb phrase})
