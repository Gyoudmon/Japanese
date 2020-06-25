#lang scribble/book

@require{../literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{@ja-title[Case 格 かく 格]}

Generally speaking, @deftech{Case} is any of the forms of a @ja-tech{noun}, @ja-tech{adjective},
or @ja-tech{pronoun} that express the semantic relation of a word to other words, especially to
@ja-tech{verbs}, in a phrase, clause, or sentence. Given that Japanese is an agglutinative language
with a rigid @ja-tech{SOV} topological structure, @ja-tech{cases} therefore play a prominent role
in marking grammatical structure as well as information structure and thematic role.

In the sense of @ja-tech{case}, the standard modern Japanese has been categorized as an
@ja-deftech["accusative language" 対格言語 たいかくげんご 宾格语言] in which case subjects of
@ja-tech{transitive verbs} and @ja-tech{intransitive verbs} share the same @ja-tech{cases},
but are distinguished from @ja-tech{cases} of objects of @ja-tech{transitive verbs}.

Japanese implements its @ja-tech{case} system by postpositional @ja-tech{particles} which
more precisely are also known as @ja-tech{case particles}. These @ja-tech{case particles}
are phonologically bound to the preceding words as the form @ja-form{@ja-tech{Substantive}
 + @ja-tech{Case Particle}} is, nevertheless, @ja-tech{case particles} are actually phrasal
clitics rather than nominal declensions so that other elements may intervene between
@ja-tech{case particles} and the marking @ja-tech{substantives} or extend the scope
over more than one noun phrases@ja-exref[interleaved-nps].

@ja-exemplify{interleaved-nps}

One of the key features of Japanese @ja-tech{case} system is based on its @ja-tech{topic}-prominent
nature@handbook-footnote{Korean and Mandarin Chinese are also topic-prominent languages}.
Multiple nominative-marked noun phrases may occure within one clause so that the initial one
may be considered as the @ja-tech{topic} and described by the remaining part of the
clause@ja-exref[hallmark:multi-noms]. This phenomenon has become a major problem for describing
Japanese @ja-tech{case} system.

@ja-exemplify{hallmark:multi-noms}

@handbook-scenario{@ja-title[#:ja-term? #false Heirarchy 階層構造 かいそうこうぞう 层次结构]}


There are 10 prominent @ja-tech{case particles} in the standard modern Japanese:
  が, を, に, の, へ, で, より, から, まで, and と.

Just as the @ja-tech{case particles}, there are 10 prominent @ja-tech{cases} in the standard modern
Japanese, but they are not one-to-one corresponding.
 
@itemlist[
 #:style 'compact
 @item{@ja-deftech[#:abbr NOM Nominative 主格 しゅかく 主格]: 〜が}
 @item{@ja-deftech[#:abbr ACC Accusative 対格 たいかく 宾格]: 〜を}
 @item{@ja-deftech[#:abbr DAT Dative 与格 よかく 与格]: 〜に}
 @item{@ja-deftech[#:abbr INSTR Instrumental 具格 ごかく 工具格]: 〜で}
 @item{@ja-deftech[#:abbr LOC Locative 処格 しょかく 场所格]: 〜で、〜に}
 @item{@ja-deftech[#:abbr ALL Allative 向格 こうかく 方向格]: 〜へ、〜に}
 @item{@ja-deftech[#:abbr ABL Ablative 奪格 だっかく 夺格]: 〜から}
 @item{@ja-deftech[#:abbr GEN Genitive 属格 ぞっかく 领格]: 〜の}
 @item{@ja-deftech[#:abbr COM Comitative 共格 きょうかく 共格]: 〜と}
 @item{@ja-deftech[#:abbr TERM Terminative 到格 とうかく 到格]: 〜まで}]

@handbook-scenario{@ja-title[Valency 結合価 けつごうか 配价]}

@handbook-reference[]
