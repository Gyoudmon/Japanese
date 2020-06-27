#lang scribble/book

@require{literacy.rkt}

@handbook-story[#:index? #true]{@ja-title[Grammar 文法 ぶんぽう 语法]}

A @ja-deftech[clause 節 せつ 分句]@handbook-footnote{Don't be confused with the 文節 which is
 the smallest unit of words that sounds natural in a @ja-tech{sentence}. A 文節 consists of
 words but is not a @ja-tech{clause}.} is a unit of grammatical organization that typically
consists of a @ja-tech[#:key "predicate"]{predication structure} along with a @ja-tech{subject}.
If a @ja-tech{clause} can stand alone as a @ja-tech{sentence}, it is a
@ja-deftech["main clause" 主節 しゅせつ 主句], otherwise it is a
@ja-deftech["subordinate clause" 従属節 じゅうぞくせつ 从句].

By the definition taught at school, a @ja-deftech[sentence 文 ぶん 句子] typically contains
a @ja-tech{subject}, a @ja-tech{predicate}, and other components as well as punctuation marks.
The @ja-tech{predicate} plays the central role and determines the number of other components.
A @ja-tech{sentence} can be any one of@handbook-footnote{This category of @ja-tech{sentences}
 is controversial, but it's not a big thing since other grammatical categories (eg. @ja-tech{case}
 and @ja-tech{topic}) are much more prominent and native than @ja-tech{sentence} itself. Besides,
 this categorization is actually learnt from English but dropped the compound-complex sentence.}:
1) a @ja-deftech["simple sentence" 単文 たんぶん 简单句] if it only consists of a single @ja-tech{main clause};
2) a @ja-deftech["complex sentence" 複文 ふくぶん 复合句] if it consists of one @ja-tech{main clause}
and one or more @ja-tech{subordinate clause}s; and
3) a @ja-deftech["compound sentence" 重文 じゅうぶん 并列句] if it consists of two or more
@ja-tech{main clause}s linked by @ja-tech{conjective particles} or other conjectives.

@include-section{grammar/case.scrbl}
@include-section{grammar/tense.scrbl}

@handbook-reference[]
