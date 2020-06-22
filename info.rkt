#lang info

(define collection 'use-pkg-name)
(define pkg-desc "Japanese Language")

(define deps '("digimon"))
(define build-deps '("digimon" "scribble-lib" "racket-doc"))

(define version "0.1")
(define pkg-authors '("WarGrey Gyoudmon Ju"))
(define test-omit-paths 'all)

(define typesettings '(["Japanese.scrbl" lualatex]))

(define samples '(["literacy/parts.scrbl" 0]

                  ["literacy/parts/verbs.scrbl" 1 _]
                  ["literacy/grammar/case.scrbl" 0 _]))
