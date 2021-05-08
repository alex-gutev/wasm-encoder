;;; values.lisp
;;;
;;; Copyright (c) 2020-2021 Alexander Gutev <alex.gutev@mail.bg>
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

(defpackage wasm-encoder/test.values
  (:use #:generic-cl
	#:wasm-encoder
	#:agutil
	#:alexandria
	#:trivia

	#:fiveam
	#:wasm-encoder/test)

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
		#:serialize-data-section))

(in-package #:wasm-encoder/test.values)


;;; Test Suite Definition

(def-suite values
    :description "Test encoding of basic values"
    :in wasm-encoder)

(in-suite values)


;;; Test Serialization of Values

(test values-unsigned-integer
  "Test serialization of unsigned integer values"

  (test-encoding stream (serialize-u32 #x18 stream) #(#x18))
  (test-encoding stream (serialize-u32 #x7D stream) #(#x7D))
  (test-encoding stream (serialize-u32 64 stream) #(64))
  (test-encoding stream (serialize-u32 #xA52 stream) #(#xD2 #x14))

  (test-encoding stream
    (serialize-u32 #xA5B23408 stream)
    #(#x88 #xE8 #xC8 #xAD #x0A))

  (test-encoding stream
    (serialize-u32 (1- (expt 2 32)) stream)
    #(#xFF #xFF #xFF #xFF #x0F)))

(test values-unsigned-integer-range-checks
  "Test range checks in serialization of unsigned integer values"

  (test-error stream (serialize-u32 (expt 2 32) stream) type-error)
  (test-error stream (serialize-u32 -10 stream) type-error))

(test values-signed-integer
  "Test serialization of signed integer values"

  (test-encoding stream (serialize-i32 #x18 stream) #(#x18))
  (test-encoding stream (serialize-i32 #x7D stream) #(#xFD #x00))
  (test-encoding stream (serialize-i32 64 stream) #(#xC0 #x00))
  (test-encoding stream (serialize-i32 #xA52 stream) #(#xD2 #x14))

  (test-encoding stream (serialize-i32 -15 stream) #(#x71))
  (test-encoding stream (serialize-i32 -1234 stream) #(#xAE #x76)))

(test values-signed-integer-range-checkes
  "Test range checks in serialization of signed integer values"

  (test-error stream (serialize-i32 (expt 2 31) stream) type-error)
  (test-error stream (serialize-i32 (- (1+ (expt 2 31))) stream) type-error))

(test values-float-single
  "Test serialization of single precision (32 bit) floating point values"

  (test-encoding stream (serialize-float 7.125 stream) #(#x00 #x00 #xE4 #x40))
  (test-encoding stream (serialize-float 12.576 stream) #(#x4c #x37 #x49 #x41))

  (test-encoding stream (serialize-float -7.125 stream) #(#x00 #x00 #xE4 #xC0))
  (test-encoding stream (serialize-float -12.576 stream) #(#x4c #x37 #x49 #xC1))

  (test-error stream (serialize-float 112.765d0 stream) type-error))

(test values-float-double
  "Test serialization of double precision (64 bit) floating point values"

  (test-encoding stream (serialize-double-float 35.0625d0 stream)
		 #(#x00 #x00 #x00 #x00 #x00 #x88 #x41 #x40))

  (test-encoding stream (serialize-double-float 163.8973d0 stream)
		 #(#x6D #x56 #x7D #xAE #xB6 #x7C #x64 #x40))


  (test-encoding stream (serialize-double-float -35.0625d0 stream)
		 #(#x00 #x00 #x00 #x00 #x00 #x88 #x41 #xC0))

  (test-encoding stream (serialize-double-float -163.8973d0 stream)
		 #(#x6D #x56 #x7D #xAE #xB6 #x7C #x64 #xC0)))

(test values-strings
  "Test serialization of strings"

  (test-encoding stream
    (serialize-string "hello" stream)
    #(#x05 #x68 #x65 #x6C #x6C #x6F))

  ;; Test non-ASCII characters
  (test-encoding stream
    (serialize-string "Уникод" stream)
    #(#x0C #xD0 #xA3 #xD0 #xBD #xD0 #xB8 #xD0 #xBA #xD0 #xBE #xD0 #xB4)))

(test values-vectors
  "Test serialization of vectors"

  (test-encoding stream
    (serialize-vector #'write-byte #(#x0A #xB0 #xCD) stream)
    #(#x03 #x0A #xB0 #xCD))

  (test-encoding stream
    (serialize-vector #'write-byte #(#x0A) stream)
    #(#x01 #x0A))

  (test-encoding stream
    (serialize-vector #'write-byte #() stream)
    #(#x00)))


;;; Test Serialization of Types

(test types-values
  "Test serialization of value types"

  (test-encoding stream (serialize-type 'i32 stream) #(#x7F))
  (test-encoding stream (serialize-type 'i64 stream) #(#x7E))
  (test-encoding stream (serialize-type 'f32 stream) #(#x7D))
  (test-encoding stream (serialize-type 'f64 stream) #(#x7C))

  (test-encoding stream (serialize-type 'funcref stream) #(#x70))
  (test-encoding stream (serialize-type 'externref stream) #(#x6F)))

(test types-values-error
  "Test errors signalled when serializing unknown types"

  (test-error stream (serialize-type 'not-a-type stream) error))

(test types-function
  "Test serialization of function types"

  (test-encoding stream

    (serialize-ftype
     (make-wasm-function-type :params '(i32 i32) :results '(i32))
     stream)

    #(#x60 #x02 #x7F #x7F #x01 #x7F))

  (test-encoding stream

    (serialize-ftype
     (make-wasm-function-type :params '(i32 f32 i64 f64) :results '(f64))
     stream)

    #(#x60 #x04 #x7F #x7D #x7E #x7C #x01 #x7C))

  (test-encoding stream

    (serialize-ftype
     (make-wasm-function-type :params '(i32 i64 i32))
     stream)

    #(#x60 #x03 #x7F #x7E #x7F #x00))

  (test-encoding stream

    (serialize-ftype
     (make-wasm-function-type :params nil :results nil)
     stream)

    #(#x60 #x00 #x00))

  (test-encoding stream
    (serialize-ftype
     (make-wasm-function-type :params '(i32 i32) :results '(funcref externref))
     stream)

    #(#x60 #x02 #x7F #x7F #x02 #x70 #x6F)))

(test types-limits
  "Test serialization of limit types"

  (test-encoding stream

    (serialize-limit
     (make-wasm-limit :min 3)
     stream)

    #(#x00 #x03))

  (test-encoding stream

    (serialize-limit
     (make-wasm-limit :min 100 :max 350)
     stream)

    #(#x01 100 #xDE #x02)))

(test types-limits-errors
  "Test errors in serialization of limit types"

  (test-error stream

    (serialize-limit
     (make-wasm-limit :min -3 :max (expt 2 35))
     stream)

    type-error))

(test types-memory
  "Test serialization of memory types"

  (test-encoding stream

    (serialize-memory
     (make-wasm-limit :min 1 :max 3)
     stream)

    #(#x01 #x01 #x03))

  (test-encoding stream

    (serialize-memory
     (make-wasm-limit :min 1)
     stream)

    #(#x00 #x01)))

(test types-tables
  "Test serialization of table types"

  (test-encoding stream
    (serialize-table
     (make-wasm-table-type :min 10)
     stream)

    #(#x70 #x00 10))

  (test-encoding stream
    (serialize-table
     (make-wasm-table-type :min 10 :max 100)
     stream)

    #(#x70 #x01 10 100))

  (test-encoding stream
    (serialize-table
     (make-wasm-table-type :type 'externref :min 10 :max 100)
     stream)

    #(#x6F #x01 10 100)))

(test types-tables-errors
  "Test errors in serialization of table types"

  (test-error stream
    (serialize-table
     (make-wasm-table-type :type 'i32 :min 10)
     stream)

    error))

(test types-globals
  "Test serialization of global variable types"

  (test-encoding stream
    (serialize-global 'i32 nil stream)

    #(#x7F #x00))

  (test-encoding stream
    (serialize-global 'i64 t stream)

    #(#x7E #x01))

  (test-encoding stream
    (serialize-global 'funcref t stream)

    #(#x70 #x01)))
