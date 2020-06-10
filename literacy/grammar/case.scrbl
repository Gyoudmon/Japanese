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
are phonologically bound to the preceding words as the form @ja-form{noun + case particle}
is, nevertheless, @tech{case particles} are actually phrasal clitics rather than nominal
declensions so that other elements may intervene between @tech{case particles} and the
marking @tech{nouns} or extend the scope over more than one noun
phrases@ja-exref[interleaved-nps].

@ja-example[
 #:tag interleaved-nps
 [太郎  と 花子  だけ が  駅  から 歩い  た.]
 [たろう - はなこ -  NOM えき ABL あるい PST]
 [Only Taro and Hanako walked from the train station.]]

One of the key features of Japanese @tech{case} system is based on its @tech{topic}-prominent
property@handbook-footnote{Korean and Mandarin Chinese are also topic-prominent languages}.
Multiple nominative-marked noun phrases may occure within one clause so that the initial one
may be considered as the @tech{topic} and described by the remaining part of the
clause@ja-exref[hallmark:multi-noms]. 

@ja-example[
 #:tag hallmark:multi-noms
 [太郎  が   母親    が   評判      が　良い "."]
 [たろう NOM ははおや NOM ひょうばん NOM いい -]
 [Taro is such that his mother has a good reputation.]]

@handbook-reference[]
