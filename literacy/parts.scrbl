#lang scribble/book

@require{literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{@ja-title["Parts of Speech" "品詞"]{ひんし}}

@deftech[#:key "PoS"]{Part of Speech} is the category of words that have similar grammatical properties.
More precisely, Japanese words are categorized based on their meanings, grammatical functions and inflections.
Given that Japanese is an agglutinative language in which case words are well formed and keep consistent
after their unions, Japanese words are well categoried into 10 - 12 schemes.

@itemlist[
 @item{@ja-deftech["Independent Words" "自立語" "じりつご"]        
  @itemlist[
 @item{@ja-deftech["Substantives" "体言" "たいげん"]
    @itemlist[
 @item{@ja-tech["Nouns" "名詞" "めいし"]
      @itemlist[
 @item{@ja-tech["Pronouns" "代名詞" "だいめいし"]}
 @item{@ja-tech["Numerals" "数詞" "すうし"]}
 ]}]}
  
 @item{@ja-deftech["Declinable Words"  "用言" "ようげん"]
    @itemlist[
 @item{@ja-tech["Verbs" "動詞" "どうし"]}
 @item{@ja-tech["Adjectives" "形容詞" "けいようし"]}
 @item{@ja-tech["Adjectival Verbs" "形容動詞" "けいようどうし"]}
 ]}

 @item{@ja-deftech["Modifiers" "修飾語" "しゅうしょくご"]
    @itemlist[
 @item{@ja-tech["Prenominals" "連体詞" "れんたいし"]}
 @item{@ja-tech["Adverbs" "副詞" "ふくし"]}
]}
 
 @item{@ja-tech["Conjunctions" "接続詞" "せつぞくし"]}
 @item{@ja-tech["Interjections" "感動詞" "かんどうし"]}
 ]}

 @item{@ja-deftech["Ancillary Words" "付属語" "ふぞくご"]
  @itemlist[
 @item[@ja-tech["Particles" "助詞" "じょし"]]
 @item[@ja-tech["Auxiliary Verbs" "助動詞" "じょどうし"]]
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
