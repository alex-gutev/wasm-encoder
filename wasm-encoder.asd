;;; wasm-encoder.asd
;;;
;;; Copyright (c) 2020 Alexander Gutev <alex.gutev@mail.bg>
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a copy
;;; of this software and associated documentation files (the "Software"), to deal
;;; in the Software without restriction, including without limitation the rights
;;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;; copies of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in all
;;; copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.


(asdf:defsystem #:wasm-encoder
  :description "Library for serializing WebAssembly modules to binary .wasm files"
  :author "Alexander Gutev <alex.gutev@mail.bg>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:generic-cl
	       #:agutil
	       #:alexandria
	       #:trivia

	       #:ieee-floats
	       #:babel
	       #:flexi-streams)

  :components ((:file "package")
               (:file "wasm-encoder"))

  :in-order-to ((asdf:test-op (asdf:test-op :wasm-encoder/test))))

(asdf:defsystem #:wasm-encoder/test
  :description "Tests for wasm-encoder."
  :author "Alexander Gutev"
  :license "MIT"
  :depends-on (#:wasm-encoder #:prove #:prove-asdf)
  :defsystem-depends-on (#:prove-asdf)

  :components ((:test-file "test"))

  :perform (asdf:test-op :after (op c)
			 (funcall (intern #.(string :run) :prove) c :reporter :fiveam)))
