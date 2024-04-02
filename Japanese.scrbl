#lang scribble/book

@require{literacy/literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-title/pkg-desc[#:documentclass "stone/documentclass.tex" #:tex-package "stone/load.tex" #:tex-CJK? #false]
@texbook-frontmatter[]

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
authors have to maintain references on their own. @texbook-prefab-name{TeX} and friends are too
cumbersome to work with directly. Racket Scribble just fits the case.

@handbook-preface-subsection{Example Realm}

In order to conveniently generate different editions for different readers, the concept @deftech{Realm}
is introduced to organize example sentences in this book. Meanwhile there are three prefabricated
@ja-tech{realm}s:

@itemlist[
 #:style 'compact
 @item{@bold{default}: Examples that can be seen in regular textbooks. Besides, this @ja-tech{realm} also
  serves as the fallback in case some specific examples cannot be found in the target one.}
 @item{@bold{STEM}: Examples in fields of science, technology, engineering, mathematics and such.}
 @item{@bold{ACG}: Examples in fields of animation, cartoon, game and such.}
 ]

Typesetting with environment variable @envvar{JAREALM} setting to @italic{realm name} generates the
target edition, say @exec{env JAREALM=STEM raco wisemon typeset}, and @ja-tech{realm} names are case
sensitive.

By convention, @ja-tech{realm}s are located in @filepath{stone/realm}. Within each @ja-tech{realm}s,
@tt{.tex} suffixed plain text files, don't confused with the @texbook-prefab-name{LaTeX}
source files, are used to organize exmaples chapter by chapter. Examples are virtually have no
namespace and therefore can be freely cross referenced in other chapters.

Typically, an example in the file consists of five parts:

@itemlist[
 #:style 'compact
 @item{@bold{Identifier}: the name that identifies the example, bracketed in @racketparenfont{[]}.
  Note that whitespaces are parts of names.}
 @item{@bold{Japanese Sentence}: the mandatory part of an example, written with the primary writting
  system(a.k.a. @ja-tech{kanji}). Tokens are separated by whitespaces.}
 @item{@bold{Ruby}: written in @ja-tech{hiragana} above kanji tokens to show their pronunciation. The
  @litchar{-} is used as the placeholder for a non-@ja-tech{kanji} token.}
 @item{@bold{Mark}: written in English below @ja-tech{kana} tokens to show their grammatical or
  pragmatical functions. Multi-marks for one token are separated by period(@racketparenfont[@._]).
  Note that marks and rubies share the second line of an example in the file.}
 @item{@bold{Translation}: the meaning of the sentence, written primarily in English. Zero or multi
  translations are also allowed. Note that @emph{no} blank lines among translations.}
 ]

For multi-sentence examples, the last four parts repeat, and sentences are separated by blank lines.
Below is the content of @filepath{preface.tex} in which examples exist for illustrating only and
should be placed in their own @ja-tech{realm}s:

@hspace[1]
@linebreak[]

@tamer-racketbox{stone/realm/default/preface.tex}

@hspace[1]
@linebreak[]

The single-sentence example with identifier @racketvalfont{| ex ss |} looks like @ja-exref[#:elem values]{ ex ss }
if it is exemplified in the book. The example counter autoincrements chapter by chapter.

@ja-exemplify['| ex ss |]

Similarly, @ja-exref[#:elem values]{ ex ms } exemplifies the multi-sentence example @racketvalfont{| ex ms |},
besides, each sentence can be referenced independently: @ja-exref[#:elem values " ex ms " a],
@ja-exref[#:elem values " ex ms " b], @etc

@ja-exemplify['| ex ms |]

@handbook-smart-table[]

@texbook-mainmatter[]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@include-section{literacy/pos.scrbl}
@include-section{literacy/lexicon.scrbl}
@include-section{literacy/category.scrbl}
@include-section{literacy/semantics.scrbl}
@include-section{literacy/pragmatics.scrbl}
@include-section{literacy/phonology.scrbl}

@texbook-appendix{Appendices}
@include-section{literacy/phonology/English.scrbl}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@texbook-backmatter[]
@; `texbook-backmatter` will unnumber following sections without affecting the header

@include-section{literacy/terminology.scrbl}
@include-section{literacy/abbreviation.scrbl}

@handbook-appendix[#:index-section? #true #:numbered? #true
 (book-bib-entry "CJLH" "The Combridge Handbook of Japanese Linguistics" (editor "Yoko Hasegawa") "Cambridge University Press" #:date "2018")
 (book-bib-entry "WBJG" "世界最強日文文法" (authors "桶田宜加" "清水祐美子") "懶鬼子英日語" #:date "2018")
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
