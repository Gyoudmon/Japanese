#lang scribble/book

@require{literacy.rkt}

@handbook-story[#:index? #true]{@ja-title[Grammar 文法 ぶんぽう 语法]}

By the definition taught at school, a @ja-deftech[sentence 文 ぶん 句子] typically contains
a @ja-tech{subject}, a @ja-tech{predicate}, an @ja-tech{object} and other components as well
as punctuation marks. The @ja-tech{predicate} plays the center role and determines the number
of other components in a @ja-tech{sentence}. Structurally speaking, a @ja-tech{sentence} is
typically associated with a @ja-tech{clause}, the basic unit of grammar, and a @deftech{clause}
can be either a @deftech[#:key "simplex"]{clause simplex} or a @deftech[#:key "complex"]{clause complex}
which is composed of two or more @ja-tech{simplex}es linked by @ja-tech{conjective particles} or
other conjectives.

@include-section{grammar/case.scrbl}
@include-section{grammar/tense.scrbl}

@handbook-reference[]
