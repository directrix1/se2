#lang scheme/base

(require scheme/gui/base "../lang/dracula.ss" "../lang/check.ss")

(begin-below
 (defun play-wav (file async)
   (play-sound file (if async #t #f))))

(provide play-wav)
