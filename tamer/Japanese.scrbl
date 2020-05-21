#lang scribble/report

@(require digimon/tamer)

@(require scribble/latex-render)

@(require "../digitama/multilanguage.rkt")

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title/pkg-desc[]

@handbook-smart-table[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section["sentence.scrbl"]

@include-section["noun.scrbl"]
@include-section["adjective.scrbl"]
@include-section["verb.scrbl"]

@include-section["phonetics.scrbl"]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:index? #true
 (bib-entry #:key      "CHoJL"
            #:title    "The Combridge Handbook of Japanese Linguistics"
            #:author   (authors "Yoko Hasegawa")
            #:location (book-location #:publisher "Cambridge University Press")
            #:date     "2018"
            #:is-book? #true)
 (bib-entry #:key      "WBJG"
            #:title    "世界最強日文文法"
            #:author   (authors "桶田宜加" "清水祐美子")
            #:location (book-location #:publisher "懶鬼子英日語")
            #:date     "2018"
            #:is-book? #true)
 (bib-entry #:key      "GJP"
            #:title    "図解日本語の原理"
            #:author   (authors "王苡晴" "井上清美")
            #:location (book-location #:publisher "懶鬼子英日語")
            #:date     "2013"
            #:is-book? #true)
 (bib-entry #:key      "CJL"
            #:title    @chinese{综合日语}
            #:author   (authors @chinese{彭广陆} @chinese{守屋三千代})
            #:location @chinese[(book-location #:publisher "北京大学出版社")]
            #:date     "2006"
            #:is-book? #true)]
