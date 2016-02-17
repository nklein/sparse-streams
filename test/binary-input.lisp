;;;; test/binary-input.lisp

(in-package #:sparse-streams-test)

(nst:def-fixtures binary-vectors ()
  (abc (map 'vector #'char-code "abcdefghijklmnopqrstuvwxyz")))

(defun read-bytes-to-string (stream &optional n)
  (map 'string
       #'code-char
       (loop :for i :from 0
             :for ch := (read-byte stream n)
             :while (or (not (null n)) ch)
             :while (or (null n) (< i n))
             :when ch
             :collecting ch)))

(defun read-byte-ranges-from-vector (vector ranges &optional n seek)
  (with-open-stream (in (flexi-streams:make-in-memory-input-stream vector))
    (with-open-stream (sub (sparse-streams:make-sparse-binary-input-stream
                            in ranges))
      (when seek
        (file-position sub seek))
      (read-bytes-to-string sub n))))

(defun read-ranges-from-vector (vector ranges &optional n seek)
  (with-open-stream (in (flexi-streams:make-in-memory-input-stream vector))
    (with-open-stream (sub (sparse-streams:make-sparse-binary-input-stream
                            in ranges))
      (when seek
        (file-position sub seek))
      (let* ((len (or n (reduce #'+ ranges :key #'second
                                :initial-value (if seek (- seek) 0))))
             (arr (make-array len :element-type '(unsigned-byte 8)))
             (got (read-sequence arr sub)))
        (map 'string #'code-char (subseq arr 0 got))))))

(nst:def-test-group binary-input-read-byte-tests (binary-vectors)
  (nst:def-test single-range-starting-at-zero (:equal "abc")
    (read-byte-ranges-from-vector abc '((0 3))))

  (nst:def-test single-range-starting-at-four (:equal "efg")
    (read-byte-ranges-from-vector abc '((4 3))))

  (nst:def-test two-ranges (:equal "abcxyz")
    (read-byte-ranges-from-vector abc '((0 3) (23 3))))

  (nst:def-test overlapping-ranges (:equal "abcbcdcdef")
    (read-byte-ranges-from-vector abc '((0 3) (1 3) (2 4))))

  (nst:def-test read-beyond-end (:err)
    (read-byte-ranges-from-vector abc '((0 3) (23 3)) 7))

  (nst:def-test range-too-big (:equal "xyz")
    (read-byte-ranges-from-vector abc '((23 1000)))))

(nst:def-test-group binary-input-read-byte-after-seek-tests (binary-vectors)
  (nst:def-test seek-to-middle-of-only-range (:equal "cde")
    (read-byte-ranges-from-vector abc '((0 5)) nil 2))

  (nst:def-test seek-to-end-of-first-range (:equal "xyz")
    (read-byte-ranges-from-vector abc '((0 3) (23 3)) nil 3))

  (nst:def-test seek-to-middle-of-second-range (:equal "yz")
    (read-byte-ranges-from-vector abc '((0 3) (23 3)) nil 4))

  (nst:def-test seek-in-overlapping-ranges (:equal "cdcdef")
    (read-byte-ranges-from-vector abc '((0 3) (1 3) (2 4)) nil 4))

  (nst:def-test seek-to-end (:equal "")
    (read-byte-ranges-from-vector abc '((0 3) (23 3)) nil 6)))


(nst:def-test-group binary-input-read-sequence-tests (binary-vectors)
  (nst:def-test single-range-starting-at-zero (:equal "abc")
    (read-ranges-from-vector abc '((0 3))))

  (nst:def-test single-range-starting-at-four (:equal "efg")
    (read-ranges-from-vector abc '((4 3))))

  (nst:def-test two-ranges (:equal "abcxyz")
    (read-ranges-from-vector abc '((0 3) (23 3))))

  (nst:def-test overlapping-ranges (:equal "abcbcdcdef")
    (read-ranges-from-vector abc '((0 3) (1 3) (2 4))))

  (nst:def-test read-beyond-sequence (:equal "abcxyz")
    (read-ranges-from-vector abc '((0 3) (23 3)) 10))

  (nst:def-test range-too-big (:equal "xyz")
    (read-ranges-from-vector abc '((23 1000)) 10)))

(nst:def-test-group binary-input-read-sequence-after-seek-tests
    (binary-vectors)
  (nst:def-test seek-to-middle-of-only-range (:equal "cde")
    (read-ranges-from-vector abc '((0 5)) nil 2))

  (nst:def-test seek-to-end-of-first-range (:equal "xyz")
    (read-ranges-from-vector abc '((0 3) (23 3)) nil 3))

  (nst:def-test seek-to-middle-of-second-range (:equal "yz")
    (read-ranges-from-vector abc '((0 3) (23 3)) nil 4))

  (nst:def-test seek-in-overlapping-ranges (:equal "cdcdef")
    (read-ranges-from-vector abc '((0 3) (1 3) (2 4)) nil 4))

  (nst:def-test seek-to-end (:equal "")
    (read-ranges-from-vector abc '((0 3) (23 3)) nil 6)))

(nst:def-test-group binary-input-nested-range-tests (binary-vectors)
  (nst:def-test read-bytes-from-nested-ranges (:equal "acy")
    (with-open-stream (in (flexi-streams:make-in-memory-input-stream abc))
      (with-open-stream (sub1 (sparse-streams:make-sparse-binary-input-stream
                               in '((0 3) (23 3))))
        (with-open-stream (sub2 (sparse-streams:make-sparse-binary-input-stream
                                 sub1 '((0 1) (2 1) (4 1))))
          (read-bytes-to-string sub2)))))

  (nst:def-test read-sequence-from-nested-ranges (:equal "acy")
    (with-open-stream (in (flexi-streams:make-in-memory-input-stream abc))
      (with-open-stream (sub1 (sparse-streams:make-sparse-binary-input-stream
                               in '((0 3) (23 3))))
        (with-open-stream (sub2 (sparse-streams:make-sparse-binary-input-stream
                                 sub1 '((0 1) (2 1) (4 1))))
          (let* ((arr (make-array 10 :element-type '(unsigned-byte 8)))
                 (got (read-sequence arr sub2)))
            (map 'string #'code-char (subseq arr 0 got))))))))
