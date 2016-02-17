
(in-package #:sparse-streams)

(defclass sparse-binary-input-stream (fundamental-binary-input-stream)
  ((underlying-stream :initarg :underlying-stream :reader underlying-stream)
   (ranges :initarg :ranges :reader ranges)
   (stream-position :initform 0 :accessor stream-position)
   (current-range :initform nil :accessor current-range)
   (room-left-in-current-range :initform 0
                               :accessor room-left-in-current-range)))

(defun sparse-input-stream-length (stream)
  (reduce #'+ (ranges stream) :key #'second))

(defun set-range-from-position (stream)
  (labels ((find-range (position ranges)
             (let ((under (underlying-stream stream)))
               (cond
                 ;; if we ran out of ranges, then we are at EOF
                 ((null ranges)
                  (setf (current-range stream) nil
                        (room-left-in-current-range stream) 0
                        (stream-position stream)
                           (sparse-input-stream-length stream))
                  ;; wish this worked, but on CCL, at least, it depends
                  ;; on file-length which is not overridable by Gray Streams.
                  #+wishes-are-horses
                  (file-position under :end))

                 (t
                  (destructuring-bind (cur &rest ranges) ranges
                    (destructuring-bind (start length) cur
                      (cond
                        ;; if there is enough room in this chunk, then
                        ;; set this one as current
                        ((< position length)
                         (setf (current-range stream) cur
                               (room-left-in-current-range stream) (- length
                                                                      position))
                         (file-position under (+ start position)))
                        ;; if there is not enough room in this chunk,
                        ;; move onto the next one
                        (t
                         (find-range (- position length) ranges))))))))))
    (cond
      ((eql (stream-position stream) :end)
       (find-range (stream-position stream) nil))
      (t
       (find-range (stream-position stream) (ranges stream))))))

(defmethod initialize-instance :after ((stream sparse-binary-input-stream)
                                       &key &allow-other-keys)
  (set-range-from-position stream))

(defun valid-range-p (range)
  (check-type range list)
  (destructuring-bind (start length) range
    (check-type start (integer 0 *))
    (check-type length (integer 1 *))
    t))

(defun make-sparse-binary-input-stream (underlying-stream ranges)
  (check-type ranges list)
  (assert (input-stream-p underlying-stream))
  (assert (not (null ranges)))
  (assert (every #'valid-range-p ranges))
  (make-instance 'sparse-binary-input-stream
                 :underlying-stream underlying-stream
                 :ranges (mapcar #'copy-seq ranges)))

(defmethod stream-file-position ((stream sparse-binary-input-stream))
  (stream-position stream))

(defmethod (setf stream-file-position) (newval
                                        (stream sparse-binary-input-stream))
  (if (eql newval :start)
      (setf (stream-position stream) 0)
      (setf (stream-position stream) newval))
  (set-range-from-position stream)
  newval)

(defmethod stream-read-byte ((stream sparse-binary-input-stream))
  (cond
    ((current-range stream)
     (prog1
         (handler-case (read-byte (underlying-stream stream))
           (end-of-file ()
             (setf (stream-file-position stream) :end)
             :eof))
       (incf (stream-position stream))
       (when (zerop (decf (room-left-in-current-range stream)))
         (set-range-from-position stream))))
    (t :eof)))


(defmethod stream-read-sequence ((stream sparse-binary-input-stream)
                                 sequence start end &key &allow-other-keys)
  (flet ((read-part (start)
           (let* ((len (min (- end start)
                            (room-left-in-current-range stream)))
                  (got (read-sequence sequence (underlying-stream stream)
                                      :start start :end (+ start len))))
             (decf got start)
             (incf (stream-position stream) got)
             (unless (plusp (decf (room-left-in-current-range stream) got))
               (set-range-from-position stream))
             (when (< got len)
               (setf (stream-file-position stream) :end))
             got)))
    (loop :with s := start
       :while (current-range stream)
       :while (< s end)
       :do (incf s (read-part s))
       :finally (return s))))
