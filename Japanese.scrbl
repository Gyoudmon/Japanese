#lang scribble/book

@require{literacy/literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title/pkg-desc[]
@handbook-texbook-front[]

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
authors have to maintain references on their own. @handbook-latex-prefab-string{TeX} and friends are too
cumbersome to work with directly. Racket Scribble just fits the case.

@handbook-preface-subsection{Example File Format}

In order to conveniently generate different editions for different readers, the concept @deftech{Realm}
is introduced to organize example sentences in this book. Meanwhile there are three prefabricated
@ja-tech{realm}s:

@itemlist[
 #:style 'compact
 @item{@emph{default}: Examples that can be seen in regular textbooks. Besides, this @ja-tech{realm} also
  serves as the fallback in case some specific examples cannot be found in the target one.}
 @item{@emph{STEM}: Examples in fields of science, technology, engineering, mathematics and such.}
 @item{@emph{ACG}: Examples in fields of animation, cartoon, game and such.}
 ]

Typeseting with environment variable @envvar{JAREALM} set to @italic{realm name} to generate the
target edition, say @exec{env JAREALM=STEM raco wisemon typeset}, and @ja-tech{realm} names are case
sensitive.

By convention, @ja-tech{realm}s are located in @filepath{stone/realm}. Within each @ja-tech{realm}s,
@tt{.tex} suffixed plain text files, don't confused with the @handbook-latex-prefab-string{LaTeX}
source files, are used to organize exmaples chapter by chapter. Examples are actual have no namespace
and therefore can be freely cross referenced in other chapters.

Typically, an example consists of four parts:

@itemlist[
 #:style 'compact
 @item{@emph{Japanese Sentence}: the mandatory part of an example, written with the primary writting
  system(a.k.a. @ja-tech{kanji}). Tokens are separated by spaces.}
 @item{@emph{Ruby}: written in @ja-tech{hiragana} above kanji tokens to show their pronunciation.
  @racketmetafont{-} is used as a placeholder of a non-@ja-tech{kanji} token.}
 @item{@emph{Mark}: written in English below @ja-tech{kana} tokens to show their grammatical functions.
  Note that marks and rubies share the second line of an example in the file.}
 @item{@emph{Translation}: the meaning of the sentence, written primarily in English. Zero or multi
  translations are allowed.}
 ]

Below is the content of @filepath{preface.tex} which exists for illustrating:

@tamer-racketbox{stone/realm/default/preface.tex}

If the multi-sentence example with id @racketidfont{multi-example id} is referenced in the book, it will
look like @ja-exref[#:elem values]{multi-example id}.

@ja-exemplify['|multi-example id|]

@handbook-smart-table[]
@handbook-texbook-main[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section{literacy/parts.scrbl}
@include-section{literacy/lexicon.scrbl}
@include-section{literacy/grammar.scrbl}
@include-section{literacy/pragmatics.scrbl}
@include-section{literacy/phonetics.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-appendix[#:index? #false
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section{literacy/terminology.scrbl}
@include-section{literacy/abbreviation.scrbl}
