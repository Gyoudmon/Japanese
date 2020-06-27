#lang scribble/book

@require{../literacy.rkt}
@require{../token.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{@ja-title[#:ja-term? #false "Postpositional Particles" 後置助詞 こうちじょし 后置助词]}

By a strict definition, a particle is a function word that has two mandatory properties: 1) incapable
of inflection; 2) has no specific lexical definition, and must be associated with another word or
@ja-tech{phrase} to impart meaning@handbook-footnote{This definition should be kept in mind in order
to distinguish the @ja-tech{particle} from other @PoS and concepts.}.

How do particles be used varies widely from one lanaguage to another. In Japanese,
@ja-deftech[#:abbr PTCL particles 助詞 じょし 助词] are suffixes or short words agglutinating @ja-tech{nouns},
@ja-tech{verbs}, @ja-tech{adjectives}, or @ja-tech{sentence}s to indicate grammatical relations. Given that
modern Japanese @ja-tech{particles} are always postpositional, the term @ja-deftech[postposition 後置詞 こうちし 后置词]
is seen to be used as a contemporary alternative to @ja-tech{particle} and compound particle.

Japanese @ja-tech{particles} are always written in @ja-tech{hiragana} regardless their @ja-tech{kanji} forms,
and read as their phonetic transcriptions are with three irragularities@handbook-footnote{As Japanese
Language evolves, phonologically indistinguishable syllables with less used kanas tend to be eliminated,
say ゐ and ゑ for instances. The pronunciation of these three particles has been changed, whereas spells
still persist, being a particle is nonetheless the only usage of を in modern Japanese.}:
@(ruby "は" @emph{wa} #:style "bigruby"), @ruby["へ" @emph{e} #:style "bigruby"], and
@ruby["を" @emph{o} #:style "bigruby"].

In the sense of formatives that are invariant in form and do not belong to other @PoS, Japanese
@ja-tech{particles} can be categorized into four types@handbook-footnote{@ja-tech{Particles} are
 multifarious and disorderly, their categorization therefore varies among perspectives or even
 linguisticians. Four of them are worthy of their own discussions.}, and many @ja-tech{particles}
belong to more than one types.

@itemlist[
 #:style 'compact

 @item{@ja-deftech["Case Particles" 格助詞 かくじょし 格助词]: @ja-tech{particles} that employed by Japanese
  @ja-tech{case} system to mark @ja-tech{substantives}. Also containing
  @ja-deftech["Parallel Particles" 并列助詞 へいりつじょし 并列助詞].}

 @item{@ja-deftech["Adverbial Particles" 副助詞 ふくじょし 副助词]: @ja-tech{particles} that indicate relationships
  between the noun @ja-tech{phrase} and the @ja-tech{predicate} to substitute or combine with @ja-tech{case particles}.
  Also containing @ja-deftech["Binding Particles" 係助詞 かかりじょし 系助词] and known as
  @ja-deftech["Indicative Particles" "取|り|立|て|助詞" "と|-|た|-|じょし" 提示助词].}
 
 @item{@ja-deftech["Conjective Particles" 接続助詞 せつぞくじょし 接续助词]: @ja-tech{particles} that join
  @ja-tech{phrase}s or @ja-tech{clause}s to indicate their coherent relations.}
 
 @item{@ja-deftech[#:abbr SFP "Sentence-Final Particles" 終助詞 しゅうじょし 语气助词]: @ja-tech{particles} that
  play a particularly important role in spoken Japanese and indicate the speaker's mannar or modal related to
  participants. Some of them are called @ja-deftech["Insertion Particles" 間投助詞 かんとうじょし 间投助词]
  since they occur sentence-medially.}

 @;item{準体助詞}
 ]

@handbook-scenario{が}

@handbook-reference[]
