;;;; sparse-streams.asd

(asdf:defsystem #:sparse-streams
  :description "Gray Streams code for subsets of another stream."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.2.20160217"
  :license "UNLICENSE"
  :depends-on (#:trivial-gray-streams)
  :in-order-to ((asdf:test-op (asdf:load-op :sparse-streams-test)))
  :perform (asdf:test-op (o c)
             (uiop:symbol-call :sparse-streams-test :run-all-tests))
  :components
  ((:static-file "README.md")
   (:module "src"
    :components ((:file "package")
                 (:file "binary-input" :depends-on ("package"))
                 (:file "character-input" :depends-on ("package"))))))

(asdf:defsystem #:sparse-streams-test
  :description "Tests for the SPARSE-STREAMS package."
  :author "Patrick Stein <pat@nklein.com>"
  :version "0.2.20160217"
  :license "UNLICENSE"
  :depends-on ((:version #:sparse-streams "0.2.20160217")
               #:flexi-streams
               #:nst)
  :components
  ((:module "test"
    :components ((:file "package")
                 (:file "binary-input" :depends-on ("package"))
                 (:file "character-input" :depends-on ("package"))
                 (:file "run" :depends-on ("package"))))))
