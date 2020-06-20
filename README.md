# Japanese Language

wargrey



## Preface

I am a native Chinese who speaks Mandarin. I have been learning English
for more than 16 years, and now I am learning Japanese as the second
foreign language. Given that I am a software engineer and very
interested in Linguistics, regular language textbooks are hardly
satisfactory. So I embarked on this project to write my own textbook.

Since practicing Chinese is not so valuable for me, I therefore choose
English as the writing language. As a result, this book targets native
English speakers and anyone who are interested in English and Japanese.
Yes, as might be expected, comparing with Chinese is also something
interesting one would find.

Microsoft Word is good but not the best one for writing rigorous books,
one mentionable reason is that authors have to maintain references on
their own.  and friends are too cumbersome to work with directly. Racket
Scribble just fits the case.

### Example File Format

In order to conveniently generate different editions for different
readers, the concept _Realm_ is introduced to organize example sentences
in this book. Meanwhile there are three prefabricated `realm`s:

* ​_default_​: Examples that can be seen in regular textbooks. Besides,
  this `realm` also serves as the fallback in case some specific
  examples cannot be found in the target one.

* ​_STEM_​: Examples in fields of science, technology, engineering,
  mathematics and such.

* ​_ACG_​: Examples in fields of animation, cartoon, game and such.

Typeseting with environment variable `JAREALM` set to _realm name_ to
generate the target edition, say `env JAREALM=STEM raco wisemon
typeset`, and `realm` names are case sensitive.

By convention, `realm`s are located in `"stone/realm"`. Within each
`realm`s, `.tex` suffixed plain text files, don’t confused with the
source files, are used to organize exmaples chapter by chapter. Examples
are actual have no namespace and therefore can be freely cross
referenced in other chapters.

Typically, an example consists of four parts:

* ​_Japanese Sentence_​: the mandatory part of an example, written with
  the primary writting system(a.k.a. `kanji`). Tokens are separated by
  spaces.

* ​_Ruby_​: written in `hiragana` above kanji tokens to show their
  pronunciation. `-` is used as a placeholder of a non-`kanji` token.

* ​_Mark_​: written in English below `kana` tokens to show their
  grammatical functions. Note that marks and rubies share the second
  line of an example in the file.

* ​_Translation_​: the meaning of the sentence, written primarily in
  English. Zero or multi translations are allowed.

Below is the content of `"preface.tex"` which exists for illustrating:

_📝 stone/realm/default/preface.tex_
```racket
 1 [single-example id]
 2 日本語   の  文         
 3 にほんご GEN ぶん        
 4 Japanese Sentence  
 5                    
 6 [multi-example id] 
 7 第   1  文           
 8 だい -  ぶん           
 9 The 1st Sentence   
10                    
11 第   2  文           
12 だい - ぶん            
13 The 2nd Sentence   
```

If the multi-sentence example with id `multi-example id` is referenced
in the book, it will look like `ex0.1`.

`Example 0.1`         
 `a` 第1文              
                      
      The 1st Sentence
 `b` 第2文              
                      
      The 2nd Sentence




