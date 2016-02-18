SPARSE-STREAMS
==============

Sometimes, it can be useful to have a stream which represents only
certain byte ranges of another stream.  This package provides such
Gray streams which represent portions of an underlying stream.

The underlying stream must support seeking.

Currently, this package only supports binary input streams whose
`element-type` is `(unsigned-byte 8)` or character input streams
(whose `element-type` is `character`).


Binary Input Streams
--------------------

The `MAKE-SPARSE-BINARY-INPUT-STREAM` takes two arguments: an
underlying binary input stream and a (non-empty) list of ranges.  Each
item in the list of ranges is a two-element list.  The first element
of each range item is the starting offset in the underlying stream for
this part of the range, and the second element of each range item is
the length of that stretch.

In the following example, the input stream is the letters of the
alphabet.  The sparse version of the stream is the first three
letters, followed again by the first three letters, followed by the
last three letters.

    (let ((alpha (map 'vector #'char-code "abcdefghijklmnopqrstuvwxyz")))
      (with-open-stream (under (flexi-streams:make-in-memory-input-stream
                                  alpha))
        ;; extract "abcabcxyz"
        (with-open-stream (in (make-sparse-binary-input-stream under
                                                               '((0 3)
                                                                 (0 3)
                                                                 (23 3))))
          (let ((seq (make-array '(9) :element-type '(unsigned-byte 8))))
            (read-sequence seq in)
            (map 'string #'code-char seq)))))

     => "abcabcxyz"

One can use a sparse binary input stream as the underlying stream to
another sparse binary input stream:

    (let ((alpha (map 'vector #'char-code "abcdefghijklmnopqrstuvwxyz")))
      (with-open-stream (under (flexi-streams:make-in-memory-input-stream
                                  alpha))
        ;; extract "abcxyz"
        (with-open-stream (in1 (make-sparse-binary-input-stream under
                                                                '((0 3)
                                                                  (23 3))))
          ;; extract "acy"
          (with-open-stream (in2 (make-sparse-binary-input-stream in1
                                                                '((0 1)
                                                                  (2 1)
                                                                  (4 1))
            (let ((seq (make-array '(3) :element-type '(unsigned-byte 8))))
              (read-sequence seq in2)
              (map 'string #'code-char seq))))

     => "acy"

Character Input Streams
-----------------------

The `MAKE-SPARSE-CHARACTER-INPUT-STREAM` takes two arguments: an
underlying character input stream and a (non-empty) list of ranges.
Each item in the list of ranges is a two-element list.  The first
element of each range item is the starting offset in the underlying
stream for this part of the range, and the second element of each
range item is the length of that stretch.

In the following example, the input stream is the letters of the
alphabet.  The sparse version of the stream is the first three
letters, followed again by the first three letters, followed by the
last three letters.

    (let ((alpha "abcdefghijklmnopqrstuvwxyz"))
      (with-input-from-string (under alpha)
        ;; extract "abcabcxyz"
        (with-open-stream (in (make-sparse-character-input-stream under
                                                               '((0 3)
                                                                 (0 3)
                                                                 (23 3))))
          (let ((seq (make-array '(9) :element-type '(unsigned-byte 8))))
            (read-sequence seq in)
            (map 'string #'identity seq)))))

     => "abcabcxyz"

One can use a sparse character input stream as the underlying stream to
another sparse character input stream:

    (let ((alpha "abcdefghijklmnopqrstuvwxyz"))
      (with-input-from-string (under alpha)
        ;; extract "abcxyz"
        (with-open-stream (in1 (make-sparse-character-input-stream under
                                                                '((0 3)
                                                                  (23 3))))
          ;; extract "acy"
          (with-open-stream (in2 (make-sparse-character-input-stream in1
                                                                '((0 1)
                                                                  (2 1)
                                                                  (4 1))
            (let ((seq (make-array '(3) :element-type '(unsigned-byte 8))))
              (read-sequence seq in2)
              (map 'string #'identity seq))))

     => "acy"

Note: Character streams currently only support seeking with
`FILE-POSITION`, reading with `READ-CHAR`, and reading with
`READ-SEQUENCE`.
