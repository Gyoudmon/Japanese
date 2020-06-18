#lang scribble/book

@require{../literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{@ja-title[Case 格 かく 格]}

@deftech{Case} is any of the forms of a @tech{noun}, @tech{adjective}, or @tech{pronoun}
that express the semantic relation of a word to other words, especially to @tech{verbs},
in a sentence.

In the sense of @tech{case}, the standard modern Japanese has been categorized as an
@ja-deftech["accusative language" 対格言語 たいかくげんご 宾格语言] which means subjects of
@tech{transitive verbs} and @tech{intransitive verbs} share the same @tech{cases},
but are distinguished from @tech{cases} of objects of @tech{transitive verbs}.

Japanese implements its @tech{case} system by postpositional @tech{particles}, which
traditionally are also known as @tech{case particles}. These @tech{case particles}
are phonologically bound to the preceding words as the form @ja-form{substantives +
 case particle} is, nevertheless, @tech{case particles} are actually phrasal clitics
rather than nominal declensions so that other elements may intervene between
@tech{case particles} and the marking @tech{substantives} or extend the scope
over more than one noun phrases@ja-exref[interleaved-nps].

@ja-example[
 #:tag interleaved-nps
 [太郎  と 花子  だけ が  駅  から 歩い  た.]
 [たろう - はなこ -  NOM えき ABL ある  PST]
 [Only Taro and Hanako walked from the train station.]]

One of the key features of Japanese @tech{case} system is based on its @tech{topic}-prominent
property@handbook-footnote{Korean and Mandarin Chinese are also topic-prominent languages}.
Multiple nominative-marked noun phrases may occure within one clause so that the initial one
may be considered as the @tech{topic} and described by the remaining part of the
clause@ja-exref[hallmark:multi-noms].

@ja-example[
 #:tag hallmark:multi-noms
 [太郎  が   母親    が   評判      が　良い.]
 [たろう NOM ははおや NOM ひょうばん NOM い]
 [Taro is such that his mother has a good reputation.]
 [Taro is such that his mother has a good reputation.]]

Just as the @tech{case particles}, there are 10 prominent @tech{cases} in the standard modern
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

@handbook-reference[]
