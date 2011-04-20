#lang scheme

(require "../private/planet.ss"
         "test-language.ss"
         "test-library.ss"
         "test-teachpacks.ss" ;; LINKS, does not TEST
         "test-modular.ss"
         "test-parse.ss"
         "test-proof.ss"
         "test-regexp.ss"
         "test-state.ss"
         "test-private.ss")
(require (schemeunit test)
         (schemeunit graphical-ui))

(test/graphical-ui
 (test-suite "Dracula"
   (test-suite "Internal Tools"
     test-private
     regexp-test-suite)
   (test-suite "ACL2 Connection"
     parse-test-suite
     proof-test-suite
     state-test-suite)
   (test-suite "Language Implementation"
     test-teachpacks
     test-library
     test-language
     test-modular))
 #f)
