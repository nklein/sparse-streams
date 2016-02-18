;;;; test/character-input.lisp

(in-package #:sparse-streams-test)

(nst:def-fixtures character-vectors ()
  (abc "abcdefghijklmnopqrstuvwxyz"))

(defun read-chars-to-string (stream &optional n)
  (map 'string
       #'identity
       (loop :for i :from 0
             :for ch := (read-char stream n)
             :while (or (not (null n)) ch)
             :while (or (null n) (< i n))
             :when ch
             :collecting ch)))

(defun read-char-ranges-from-string (string ranges &optional n seek)
  (with-input-from-string (in string)
    (with-open-stream (sub (sparse-streams:make-sparse-character-input-stream
                            in ranges))
      (when seek
        (file-position sub seek))
      (read-chars-to-string sub n))))

(defun read-ranges-from-string (string ranges &optional n seek)
  (with-input-from-string (in string)
    (with-open-stream (sub (sparse-streams:make-sparse-character-input-stream
                            in ranges))
      (when seek
        (file-position sub seek))
      (let* ((len (or n (reduce #'+ ranges :key #'second
                                :initial-value (if seek (- seek) 0))))
             (arr (make-array len :element-type 'character))
             (got (read-sequence arr sub)))
        (map 'string #'identity (subseq arr 0 got))))))

(nst:def-test-group character-input-read-char-tests (character-vectors)
  (nst:def-test single-range-starting-at-zero (:equal "abc")
    (read-char-ranges-from-string abc '((0 3))))

  (nst:def-test single-range-starting-at-four (:equal "efg")
    (read-char-ranges-from-string abc '((4 3))))

  (nst:def-test two-ranges (:equal "abcxyz")
    (read-char-ranges-from-string abc '((0 3) (23 3))))

  (nst:def-test overlapping-ranges (:equal "abcbcdcdef")
    (read-char-ranges-from-string abc '((0 3) (1 3) (2 4))))

  (nst:def-test read-beyond-end (:err)
    (read-char-ranges-from-string abc '((0 3) (23 3)) 7))

  (nst:def-test range-too-big (:equal "xyz")
    (read-char-ranges-from-string abc '((23 1000)))))

(nst:def-test-group character-input-read-char-after-seek-tests
    (character-vectors)
  (nst:def-test seek-to-middle-of-only-range (:equal "cde")
    (read-char-ranges-from-string abc '((0 5)) nil 2))

  (nst:def-test seek-to-end-of-first-range (:equal "xyz")
    (read-char-ranges-from-string abc '((0 3) (23 3)) nil 3))

  (nst:def-test seek-to-middle-of-second-range (:equal "yz")
    (read-char-ranges-from-string abc '((0 3) (23 3)) nil 4))

  (nst:def-test seek-in-overlapping-ranges (:equal "cdcdef")
    (read-char-ranges-from-string abc '((0 3) (1 3) (2 4)) nil 4))

  (nst:def-test seek-to-end (:equal "")
    (read-char-ranges-from-string abc '((0 3) (23 3)) nil 6)))


(nst:def-test-group character-input-read-sequence-tests (character-vectors)
  (nst:def-test single-range-starting-at-zero (:equal "abc")
    (read-ranges-from-string abc '((0 3))))

  (nst:def-test single-range-starting-at-four (:equal "efg")
    (read-ranges-from-string abc '((4 3))))

  (nst:def-test two-ranges (:equal "abcxyz")
    (read-ranges-from-string abc '((0 3) (23 3))))

  (nst:def-test overlapping-ranges (:equal "abcbcdcdef")
    (read-ranges-from-string abc '((0 3) (1 3) (2 4))))

  (nst:def-test read-beyond-sequence (:equal "abcxyz")
    (read-ranges-from-string abc '((0 3) (23 3)) 10))

  (nst:def-test range-too-big (:equal "xyz")
    (read-ranges-from-string abc '((23 1000)) 10)))

(nst:def-test-group character-input-read-sequence-after-seek-tests
    (character-vectors)
  (nst:def-test seek-to-middle-of-only-range (:equal "cde")
    (read-ranges-from-string abc '((0 5)) nil 2))

  (nst:def-test seek-to-end-of-first-range (:equal "xyz")
    (read-ranges-from-string abc '((0 3) (23 3)) nil 3))

  (nst:def-test seek-to-middle-of-second-range (:equal "yz")
    (read-ranges-from-string abc '((0 3) (23 3)) nil 4))

  (nst:def-test seek-in-overlapping-ranges (:equal "cdcdef")
    (read-ranges-from-string abc '((0 3) (1 3) (2 4)) nil 4))

  (nst:def-test seek-to-end (:equal "")
    (read-ranges-from-string abc '((0 3) (23 3)) nil 6)))

(nst:def-test-group character-input-nested-range-tests (character-vectors)
  (nst:def-test read-chars-from-nested-ranges (:equal "acy")
    (with-input-from-string (in abc)
      (with-open-stream (sub1
                         (sparse-streams:make-sparse-character-input-stream
                          in '((0 3) (23 3))))
        (with-open-stream (sub2
                           (sparse-streams:make-sparse-character-input-stream
                            sub1 '((0 1) (2 1) (4 1))))
          (read-chars-to-string sub2)))))

  (nst:def-test read-sequence-from-nested-ranges (:equal "acy")
    (with-input-from-string (in abc)
      (with-open-stream (sub1
                         (sparse-streams:make-sparse-character-input-stream
                          in '((0 3) (23 3))))
        (with-open-stream (sub2
                           (sparse-streams:make-sparse-character-input-stream
                            sub1 '((0 1) (2 1) (4 1))))
          (let* ((arr (make-array 10 :element-type 'character))
                 (got (read-sequence arr sub2)))
            (map 'string #'identity (subseq arr 0 got))))))))
