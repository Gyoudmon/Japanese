#lang scribble/report

@(require digimon/tamer)

@require{literacy/literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title/pkg-desc[]

@; The preface will always be displayed after the table of contents if it is `include-section`ed.
@handbook-preface-section{Preface}

I am a native Chinese who speaks Mandarin. I have been learning English for more than 16 years,
and now I am learning Japanese as the second foreign language. Given that I am a software engineer
and very interested in Linguistics, regular language textbooks are hardly satisfactory. So I embarked
on this project to write my own textbook.

Since practicing Chinese is not so valuable for me, I therefore choose English as the writing language.
As a result, this book targets native English speakers and anyone who are interested in English and
Japanese. Yes, as might be expected, comparing with Chinese is also something interesting one would find.

Microsoft Word is good but not the best one for writing rigorous books, one mentionable reason is that
authors have to maintain references on their own. LaTex is too cumbersome to work with directly.
Racket Scribble just fits the case.

Discussions and Suggestions are welcome.

@handbook-smart-table[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section{literacy/parts.scrbl}
@include-section{literacy/lexicon.scrbl}
@include-section{literacy/grammar.scrbl}
@include-section{literacy/pragmatics.scrbl}
@include-section{literacy/phonetics.scrbl}

@include-section{literacy/tables.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:index? #true
 (bib-entry #:key      "CJLH"
            #:title    "The Combridge Handbook of Japanese Linguistics"
            #:author   (editor "Yoko Hasegawa")
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
            #:is-book? #true)
 (bib-entry #:key      "JL"
            #:title    @chinese{日语语言学}
            #:author   (editor @chinese{翟东娜})
            #:location @chinese[(book-location #:publisher "高等教育出版社")]
            #:date     "2006"
            #:is-book? #true)]
