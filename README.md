# Japanese Language

WarGrey Gyoudmon Ju



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

### Example Realm

In order to conveniently generate different editions for different
readers, the concept _Realm_ is introduced to organize example sentences
in this book. Meanwhile there are three prefabricated realms:

* **default**: Examples that can be seen in regular textbooks. Besides,
  this realm also serves as the fallback in case some specific examples
  cannot be found in the target one.

* **STEM**: Examples in fields of science, technology, engineering,
  mathematics and such.

* **ACG**: Examples in fields of animation, cartoon, game and such.

Typeseting with environment variable `JAREALM` set to _realm name_ to
generate the target edition, say `env JAREALM=STEM raco wisemon
typeset`, and realm names are case sensitive.

By convention, realms are located in `"stone/realm"`. Within each
realms, `.tex` suffixed plain text files, don’t confused with the
source files, are used to organize exmaples chapter by chapter. Examples
are virtually have no namespace and therefore can be freely cross
referenced in other chapters.

Typically, an example in the file consists of five parts:

* **Identifier**: the name that identifies the example, bracketed in
  `[]`. Note that whitespaces are parts of names.

* **Japanese Sentence**: the mandatory part of an example, written with
  the primary writting system(a.k.a. kanji). Tokens are separated by
  whitespaces.

* **Ruby**: written in hiragana above kanji tokens to show their
  pronunciation. `-` is used as a placeholder of a non-kanji token.

* **Mark**: written in English below kana tokens to show their
  grammatical or pragmatical functions. Multi-marks for one token are
  separated by period(`.`). Note that marks and rubies share the second
  line of an example in the file.

* **Translation**: the meaning of the sentence, written primarily in
  English. Zero or multi translations are allowed. Note that ​_no_​
  blank lines among translations.

For multi-sentence examples, the last four parts repeat, and sentences
are separated by blank lines. Below is the content of `"preface.tex"` in
which examples exist for illustrating only and should be placed in their
own realms:

 

_📝 stone/realm/default/preface.tex_
```racket
 1 [ ex ss ]                                                          
 2 ねえ、どうも 由紀子 が  返事   を  出さ なかっ た ようだ  よ                              
 3 -           ゆきこ NOM へんじ ACC  だ   NEG  PST Belief Assert            
 4 Hey，It seems that Yukiko didn't send a reply，(I tell you)          
 5                                                                    
 6 [ ex ms ]                                                          
 7 悟空   は  クリリン と    天下一武道会         に   参加 した                         
 8 ごくう TOP -　　　　COM　てんかいちぶどうかい　DAT さんか PST                            
 9 Goku participated in the World Martial Arts Tournament with Krillin
10                                                                    
11 音楽    は   感覚 　 の   数学  であり、 数学    は  理性   の  音楽  である               
12 おんがく TOP かんかく GEN すうがく COP すうがく TOP りせい GEN おんがく COP               
13 Music is the mathematics of sense                                  
14 Mathematics is the music of reason                                 
```

 

The single-sentence example with identifier `| ex ss |` looks like
`ex0.1` if it is exemplified in the book. The example counter
autoincrements chapter by chapter.

`Example 0.1`                                                     
ねえ、どうも由紀子ゆきこが`NOM`返事へんじを`ACC`出さだなかっ`NEG`た`PST`ようだ`Belief`よ`Assert`
                                                                  
 Hey，It seems that Yukiko didn't send a reply，(I tell you)        

Similarly, `ex0.2` exemplifies the multi-sentence example `| ex ms |`,
besides, each sentence can be referenced independently: `ex0.2a`,
`ex0.2b`, etc.

`Example 0.2`                                                                    
 `a` 悟空ごくうは`TOP`クリリンと`COM`天下一武道会てんかいちぶどうかいに`DAT`参加さんかした`PST`                     
                                                                                 
      Goku participated in the World Martial Arts Tournament with Krillin        
 `b` 音楽おんがくは`TOP`感覚かんかくの`GEN`数学すうがくであり`COP`、数学すうがくは`TOP`理性りせいの`GEN`音楽おんがくである`COP`
                                                                                 
      Music is the mathematics of sense                                          
      Mathematics is the music of reason                                         




