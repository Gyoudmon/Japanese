#lang scribble/book

@require{literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story[#:index? #true]{@ja-title[#:key "PoS" "Parts of Speech" 品詞 ひんし 词类]}

@deftech[#:key "PoS"]{Part of Speech} is the category of words that have similar grammatical properties. More precisely,
Japanese words are categorized based on their meanings, grammatical functions and inflections. Given that
Japanese is an agglutinative language in which case words are well formed and keep consistent after their
unions, Japanese words are well categoried into 10 - 12 groups.

@itemlist[
 @item{@ja-deftech["Independent Words" 自立語 じりつご 实词]
  @itemlist[
 @item{@ja-deftech[Substantives 体言 たいげん 体言]
    @itemlist[
 @item{@ja-tech[Nouns 名詞 めいし]
      @itemlist[
 @item{@ja-tech[Pronouns 代名詞 だいめいし]}
 @item{@ja-tech[Numerals 数詞 すうし]}
 ]}]}
  
 @item{@ja-deftech["Declinable Words" 用言 ようげん 用言]
    @itemlist[
 @item{@ja-tech[Verbs 動詞 どうし]}
 @item{@ja-tech[Adjectives 形容詞 けいようし]}
 @item{@ja-tech["Adjectival Verbs" 形容動詞 けいようどうし]}
 ]}

 @item{@ja-deftech[Modifiers 修飾語 しゅうしょくご 修饰语]
    @itemlist[
 @item{@ja-tech[Prenominals 連体詞 れんたいし]}
 @item{@ja-tech[Adverbs 副詞 ふくし]}
]}
 
 @item{@ja-tech[Conjunctions 接続詞 せつぞくし]}
 @item{@ja-tech[Interjections 感動詞 かんどうし]}
 ]}

 @item{@ja-deftech["Ancillary Words" 付属語 ふぞくご 虚词]
  @itemlist[
 @item[@ja-tech[Particles 助詞 じょし]]
 @item[@ja-tech["Auxiliary Verbs" 助動詞 じょどうし]]
 ]}]

These categories are typically taught at school in Japan. However categorization schemes may vary due
to different grammatically perspectives. Nowadays, textbooks tend to use modern categorization scheme.
For example, the @tech{Adjectives} and @tech{Adjectival Verbs} have been merged, with being named
as @tech{イ-adjective} and @tech{ナ-adjective} respectively. These changes may not bad, despite the
fact that new categories may be given meaningless names, say @tech{カ変 conjugation verb} and
@tech{サ変 conjugation verb} are merged as @italic{the Type-III verb} which is an awful name
@handbook-footnote{This might be a personal preference as what I have learnt from Software Engineering is
 that do not use meaningless words (such as @italic{a}, @italic{b}, @italic{c}, @italic{d}) to name variables
 since they make the code less readable for others and even yourself in the future}. Thus, this book follows
the modern scheme but drops all meaningless names.

@include-section{parts/nouns.scrbl}
@include-section{parts/verbs.scrbl}
@include-section{parts/adjectives.scrbl}
@include-section{parts/adverbs.scrbl}
@include-section{parts/particles.scrbl}
@include-section{parts/interjections.scrbl}
@include-section{parts/conjunctions.scrbl}

@handbook-reference[]
