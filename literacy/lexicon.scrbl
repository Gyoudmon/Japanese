#lang scribble/book

@require{literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{@chapter-title["Lexicon" "語彙"]{ごい}}

Here lists technical terms that commonly seen in Japanese Grammer.
 
@itemlist[
 @item{@define-term["Independent Word" "自立語" "じりつご"]

  Any words that having lexical meaning and standing by themselves as a part of a sentence.
        
  @itemlist[
 @item{@define-term["Substantive" "体言" "たいげん"]
        
    A term used to denote @tech{nouns} and @tech{pronouns} collectively. It identifies any of a class of
    people, places, or things, or to name a particular one of these.

    It performs the main part of the @define-term["Subject" "主語" "しゅご"]
    or @define-term["Object" "目的語" "もくてきご"] of a sentence.}
  
 @item{@define-term["Conjugable Word" "用言" "ようげん"]

    A term used to denote @tech{verbs} and @tech{adjectives} collectively. It describes an action,
    state, property, or occurrence.

    It performs the main part of the @define-term["Predicate" "述語" "じゅつご"] of a sentence.

    It consists of a stem and a suffix.}
  
 @item{@define-term["Prenominal Modifier" "連体修飾語" "れんたいしゅうしょくご"]

   A word that preceding and modifying a @tech{substantive}.}
 ]}

 @item{@define-term["Auxiliary Word" "付属語" "ふぞくご"]

  Any words that having grammatical function and agglutinating an @tech{independent word}.}
 ]

@include-section{lexicon/nouns.scrbl}
@include-section{lexicon/verbs.scrbl}
@include-section{lexicon/adjectives.scrbl}
@include-section{lexicon/adverbs.scrbl}
@include-section{lexicon/particles.scrbl}
@include-section{lexicon/interjections.scrbl}
@include-section{lexicon/conjunctions.scrbl}
@include-section{lexicon/mimetics.scrbl}

@handbook-reference[]
