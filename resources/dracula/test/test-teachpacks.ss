#lang scheme

(require
 "../private/planet.ss"
 (lib "unit.ss")
 (prefix-in audio: "../teachpacks/audio.ss")
 (prefix-in avl-rational-keys: "../teachpacks/avl-rational-keys.ss")
 (prefix-in binary-io: "../teachpacks/binary-io-utilities.ss")
 (prefix-in io: "../teachpacks/io-utilities.ss")
 (prefix-in list: "../teachpacks/list-utilities.ss")
 (prefix-in testing: "../teachpacks/testing.ss")
 (prefix-in world: "../teachpacks/world.ss")
 (prefix-in doublecheck: "../teachpacks/doublecheck.ss"))
(require (schemeunit test))

(provide test-teachpacks)

(define test-teachpacks
  (test-suite "Teachpacks"
    (test-case "audio")
    (test-case "avl-rational-keys")
    (test-case "binary-io-utilities")
    (test-case "io-utilities")
    (test-case "list-utilities")
    (test-case "testing")
    (test-case "world")
    (test-case "doublecheck")))
