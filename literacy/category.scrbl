#lang scribble/book

@require{literacy.rkt}

@handbook-story[#:index? #true]{@ja-title[#:ja-term? #false "Grammatical Category" 文法範疇 ぶんぽうはんちゅう 语法范畴]}

A @ja-deftech[clause 節 せつ 分句]@handbook-footnote{Don't be confused with the 文節 which is
 the smallest unit of words that sounds natural in a @ja-tech{sentence}. A 文節 consists of
 words but is not a @ja-tech{clause}. It is useful in the @italic{Natural Language Processing}.}
is a unit of grammatical organization that typically consists of a
@ja-tech[#:key "predicate"]{predication structure} along with an omittable @ja-tech{subject}. If a
@ja-tech{clause} can stand alone as a @ja-tech{sentence}, it is a
@ja-deftech["main clause" 主節 しゅせつ 主句], otherwise it is a
@ja-deftech["subordinate clause" 従属節 じゅうぞくせつ 从句].

By the definition taught at school, a @ja-deftech[sentence 文 ぶん 句子] typically contains
a @ja-tech{subject}, a @ja-tech{predicate}, and other components as well as punctuation marks.
A @ja-tech{sentence} can be any one of@handbook-footnote{This category of @ja-tech{sentences}
 is controversial, but it's not a big thing since other grammatical categories (eg. @ja-tech{case}
 and @ja-tech{topic}) are much more prominent and native than @ja-tech{sentence} itself.}:
1) a @ja-deftech["simple sentence" 単文 たんぶん 简单句] if it only consists of a single @ja-tech{main clause};
2) a @ja-deftech["compound sentence" 並列文 へいれつぶん 主从复句] if it consists of two or more
@ja-tech{main clause}s linked by @ja-tech{conjective particles} or other @ja-tech{conjections}; and
3) a @ja-deftech["complex sentence" "嵌|め|込|み|文" "は|-|こ|-|ぶん" 嵌入复句] if it consists of one
@ja-tech{main clause} and one or more @ja-tech{subordinate clause}s.

Japanese @ja-tech{sentence} has its special properties that worthy of their own sections, here only
briefly listing some common components of a @ja-tech{clause}.

@itemlist[
 #:style 'compact

 @item{@ja-deftech[Subject 主語 しゅご 主语]:
  One of the two basic components of a @ja-tech{clause}, typically acted by @ja-tech{substantive},
  and being the element about which the rest of the clause is predicated. Japanese @ja-tech{subject}
  is ususally refered to the @deftech{logical subject} which is the actual agent of the @ja-tech{predicate}.}
 
 @item{@ja-deftech[Predicate 述語 じゅつご 谓语]:
  The essential component of a @ja-tech{clause} that state something about the @ja-tech{subject}, acted
  by @ja-tech{verb}, @ja-tech{adjective} or form of @ja-form{@ja-tech{substantive} + @ja-tech{copula}},
  followed by a vast multifarious content (e.g. @ja-tech{auxiliary verb}, @ja-tech{sentence-final particle}).
  In manay situations, @ja-tech{predicate} can stand alone on its own in Japanese.}
 
 @item{@ja-deftech[Object 目的語 もくてきご 宾语]:
  The basic component of a @ja-tech{clause} for any @ja-tech{predicate} that acted by a
  @ja-tech{transitive verb}, typically acted by @ja-tech{substantive}, and being the target of that
  @ja-tech{predicate}. There is no clear boundary between the @ja-tech{object} and @ja-tech{complement}
  in Japanese.}
 
 @item{@ja-deftech[Complement 補語 ほご 补语]@handbook-footnote{The @ja-tech{complement} is a controversial term,
   its definition used here is the one taught at school.}:
  An optional component of a @ja-tech{clause}, typically formed by nounal element that other than the
  @ja-tech{object} and @ja-tech{adverbial modifier}, to complete the meaning of the @ja-tech{predicate}.}
 
 @item{@ja-deftech["Attributive Modifier" 連体修飾語 れんたいしゅうしょくご 定语]:
  An optional component of a @ja-tech{clause}, typically acted by @ja-tech{adjective}, the
  @ja-tech{attributive form}, and elements marked by the adnominal modifier の, modifying the @ja-tech{substantive}.
  Japanese makes heavy use of @ja-tech{attributive modifier}s and some of them may have complicated structures.}
 
 @item{@ja-deftech["Adverbial Modifier" 連用修飾語 れんようしゅうしょくご 状语]:
  An optional component of a @ja-tech{clause}, typically acted by @ja-tech{adverb}, @ja-tech{numeral},
  some @ja-tech{case particles} and the @ja-tech{continuative form}, modifying the @ja-tech{declinable word}
  as well as the @ja-tech{predicate} or entire @ja-tech{sentence} to make them more detailed by expressing
  place, time, circumstance, manner, cause, degree, @etc}

 @item{@ja-deftech[Independent 独立語 どくりつご 独立语]@handbook-footnote{What is the offical name of this term in English?}:
  An optional component of a @ja-tech{sentence} that absolutely independent of other components yet contributing
  to expression, typically containing address, parenthesis, @ja-tech{interjection}, @ja-tech{mimetics}.}
 ]

@include-section{category/case.scrbl}
@include-section{category/topic.scrbl}
@include-section{category/tense.scrbl}

@handbook-reference[]
