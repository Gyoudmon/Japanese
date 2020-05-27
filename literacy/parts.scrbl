#lang scribble/book

@require{literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{@chapter-title["Parts of Speech" "品詞"]{ひんし}}

Japanese is an agglutinative language and has its own lexicon terms,
some common terms may also have different semantic meanings.
 
@itemlist[
 @item{@define-term["Independent Words" "自立語" "じりつご"]

  Any words that having lexical meaning and standing by themselves as a part of a sentence.
        
  @itemlist[
 @item{@define-term["Substantives" "体言" "たいげん"]
        
    A term used to denote @tech{nouns} and @tech{pronouns} collectively. It identifies any of a class of
    people, places, or things, or to name a particular one of these.

    It performs the main part of the @define-term["Subjects" "主語" "しゅご"]
    or @define-term["Objects" "目的語" "もくてきご"] of a sentence.}
  
 @item{@define-term["Conjugable Words" "用言" "ようげん"]

    A term used to denote @tech{verbs} and @tech{adjectives} collectively. It describes an action,
    state, property, or occurrence.

    It performs the main part of the @define-term["Predicates" "述語" "じゅつご"] of a sentence.

    It consists of a stem and a suffix.}
 
 @item{@define-term["Prenominal Modifiers" "連体修飾語" "れんたいしゅうしょくご"]

   A word that preceding and modifying a @tech{substantive}.}

 @item{@define-term["Adverbs" "副詞" "ふくし"]

    A role that played by other words to modify a @tech{conjugable word}.

    It is a subset of @ruby["連用修飾語" "れんようしゅうしょくご"].}

 @item{@define-term["Conjunctions" "接続詞" "せつぞくし"]}
 
 @item{@define-term["Interjections" "感動詞" "かんどうし"]}
 ]}

 @item{@define-term["Ancillary Words" "付属語" "ふぞくご"]

  Any words that having grammatical function and agglutinating an @tech{independent word}.

  @itemlist[
 @item[@define-term["Particles" "助詞" "じょし"]]
 @item[@define-term["Auxiliary Verbs" "助動詞" "じょどうし"]]
 ]}
 ]

@include-section{parts/nouns.scrbl}
@include-section{parts/verbs.scrbl}
@include-section{parts/adjectives.scrbl}
@include-section{parts/adverbs.scrbl}
@include-section{parts/particles.scrbl}
@include-section{parts/interjections.scrbl}
@include-section{parts/conjunctions.scrbl}

@handbook-reference[]
