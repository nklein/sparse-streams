;;; src/package.lisp

(defpackage #:sparse-streams
  (:use #:cl #:trivial-gray-streams)
  (:export #:make-sparse-binary-input-stream)
  (:export #:make-sparse-character-input-stream))
