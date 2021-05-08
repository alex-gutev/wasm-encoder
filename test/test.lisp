;;; test.lisp
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

(defpackage wasm-encoder/test
  (:use #:generic-cl
	#:wasm-encoder
	#:agutil
	#:alexandria
	#:trivia

	#:fiveam)

  (:shadowing-import-from #:generic-cl
                          #:emptyp
                          #:multiply
                          #:accumulate)

  (:import-from #:flexi-streams
                #:with-output-to-sequence)

  (:import-from #:wasm-encoder
		#:serialize-u32
		#:serialize-i32

		#:serialize-float
		#:serialize-double-float

		#:serialize-string
		#:serialize-vector

		#:serialize-type
		#:serialize-ftype

		#:serialize-limit
		#:serialize-memory
		#:serialize-table

		#:serialize-global

		#:serialize-instruction
		#:serialize-instructions
		#:serialize-expression

		#:serialize-types
		#:serialize-imports
		#:serialize-function-types
		#:serialize-table-types
		#:serialize-memory-types
		#:serialize-global-section
		#:serialize-export-section
		#:serialize-start-section
		#:serialize-table-elements
		#:serialize-functions
		#:serialize-data-section)

  (:export
   #:test-encoding
   #:test-error
   #:test-encode-instruction
   #:wasm-encoder
   #:test-wasm-encoder))

(in-package #:wasm-encoder/test)


;;; Test Utilities

(defmacro test-encoding (stream &body (form expected))
  `(is (= ,expected
	  (with-output-to-sequence (,stream) ,form))))

(defmacro test-error (stream &body (form type))
  `(signals ,type
     (with-output-to-sequence (,stream) ,form)
     ,(format nil "~a results in ~a" form type)))

(defmacro test-encode-instruction (op &rest opcodes)
  `(test-encoding stream
     (serialize-instruction ',op stream)
     #(,@opcodes)))


;;; Test Suite Definition

(def-suite wasm-encoder
    :description "Master test suite for wasm-encoder")

(in-suite wasm-encoder)

(defun test-wasm-encoder ()
  (run! 'wasm-encoder))
