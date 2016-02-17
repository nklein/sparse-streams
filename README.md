SPARSE-STREAMS
==============

Sometimes, it can be useful to have a stream which represents only
certain byte ranges of another stream.  This package provides such
Gray streams which represent portions of an underlying stream.

Currently, this package only supports binary input streams
whose `element-type` is `(unsigned-byte 8)`.


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
        (with-open-stream (in (make-sparse-binary-input-stream under
                                                               '((0 3)
                                                                 (0 3)
                                                                 (23 3))))
          (let ((seq (make-array '(9) :element-type '(unsigned-byte 8))))
            (read-sequence seq in)
            (map 'string #'code-char seq)))))

     => "abcabcxyz"
