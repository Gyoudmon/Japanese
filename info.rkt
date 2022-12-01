#lang info

(define collection 'use-pkg-name)
(define pkg-desc "Japanese Language")

(define deps '("digimon"))
(define build-deps '("digimon" "scribble-lib" "racket-doc"))

(define version "0.1")
(define pkg-authors '("WarGrey Gyoudmon Ju"))
(define test-omit-paths 'all)

(define typesettings '(["Japanese.scrbl" lualatex #px#"realm/\\w+/\\w+[.]tex$"]))

(define literacy-samples '(["literacy/pos.scrbl" 0]

                           ["literacy/pos/verb.scrbl" 1 _]
                           ["literacy/category/case.scrbl" 0 _]
                           ["literacy/phonology/ipa.scrbl" 0 _]))
