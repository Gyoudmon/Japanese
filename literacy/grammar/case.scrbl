#lang scribble/book

@require{../literacy.rkt}

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@handbook-story{@ja-title["Case" "格"]{かく}}

@deftech{Case} is any of the forms of a @tech{noun}, @tech{adjective}, or @tech{pronoun}
that express the semantic relation of a word to other words, especially to @tech{verbs},
in a sentence.

In the sense of @tech{case}, the standard modern Japanese has been categorized as an
@ja-deftech["accusative language" "対格言語"]{たいかくげんご} which means subjects of
@tech{transitive verbs} and @tech{intransitive verbs} share the same @tech{cases}, but are
distinguished from @tech{cases} of objects of @tech{transitive verbs}.

Japanese implements its @tech{case} system by postpositional @tech{particles}, which
traditionally are also known as @ja-deftech["case particles" "格助詞"]{かくじょし}. These
@tech{case particles} are phonologically bound to the preceding words as the form
@ja-form{noun + case particle} is, nevertheless, the @tech{case particles} are phrasal
clitics rather than nominal declensions so that other elements may intervene between
the @tech{case particles} and the marking @tech{nouns} or extend the scope over more
than one noun phrases.

@ja-example[
 [太郎  と 花子  だけ が  駅  から 歩い  た]
 [たろう - はなこ -  NOM えき ABL あるい PST]
 ]{Only Taro and Hanako walked from the train station}

@;handbook-scenario{Categorization}

@handbook-reference[]
