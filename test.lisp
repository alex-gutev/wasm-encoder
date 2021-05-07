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
		#:serialize-data-section))

(in-package #:wasm-encoder/test)


;;; Test Utilities

(defmacro test-encoding (stream &body (form expected))
  `(is (= ,expected
	  (with-output-to-sequence (,stream) ,form))))

(defmacro test-error (stream &body (form type))
  `(signals ,type
     (with-output-to-sequence (,stream) ,form)
     ,(format nil "~a results in ~a" form type)))

(defmacro test-encode-instruction (op opcode)
  `(test-encoding stream
     (serialize-instruction ',op stream)
     #(,opcode)))


;;; Test Suite Definition

(def-suite wasm-encoder
    :description "Master test suite for wasm-encoder")

(in-suite wasm-encoder)

(defun test-wasm-encoder ()
  (run! 'wasm-encoder))


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

(test types-globals
  "Test serialization of global variable types"

  (test-encoding stream
    (serialize-global 'i32 nil stream)

    #(#x7F #x00))

  (test-encoding stream
    (serialize-global 'i64 t stream)

    #(#x7E #x01)))


;;; Test Serialization of Instructions

;;;; Control Instructions

(test instruction-control-unreachable
  "Test serialization of instruction UNREACHABLE"

  (test-encoding stream
    (serialize-instruction 'unreachable stream)
    #(#x00)))

(test instruction-control-nop
  "Test serialization of instruction NOP"

  (test-encoding stream
    (serialize-instruction 'nop stream)
    #(#x01)))

(test instruction-control-return
  "Test serialization of instruction RETURN"

  (test-encoding stream
    (serialize-instruction 'return stream)
    #(#x0F)))

;;;; Branch Instructions

(test instruction-branch-br
  "Test serialization of instruction BR"

  (test-encoding stream
    (serialize-instruction '(br 0) stream)
    #(#x0C 0))

  (test-encoding stream
    (serialize-instruction '(br 10) stream)
    #(#x0C 10))

  (test-encoding stream
    (serialize-instruction '(br 300) stream)
    #(#x0C #xAC #x02)))

(test instruction-branch-br-errors
  "Test errors in serialization of instruction BR"

  (test-error stream
    (serialize-instruction '(br -10) stream)
    type-error))

(test instruction-branch-br_if
  "Test serialization of instruction BR_IF"

  (test-encoding stream
    (serialize-instruction '(br_if 0) stream)
    #(#x0D 0))

  (test-encoding stream
    (serialize-instruction '(br_if 10) stream)
    #(#x0D 10))

  (test-encoding stream
    (serialize-instruction '(br_if 300) stream)
    #(#x0D #xAC #x02)))

(test instruction-branch-br_if-errors
  "Test errors in serialization of instruction BR_IF"

  (test-error stream
    (serialize-instruction '(br_if -10) stream)
    type-error))

(test instruction-branch-br_table
  "Test serialization of instruction BR_TABLE"

  (test-encoding stream
    (serialize-instruction '(br_table 1 0 5) stream)

    #(#x0E #x02 1 0 5))

  (test-encoding stream
    (serialize-instruction '(br_table 1 5 300 0 4) stream)

    #(#x0E #x04 1 5 #xAC #x02 0 4)))

(test instruction-branch-br_table-errors
  "Test errors in serialization of instruction BR_TABLE"

  (test-error stream
    (serialize-instruction '(br_table 1 6 -3 4) stream)
    type-error))

;;;; Call Instructions

(test instruction-call
  "Test serialization of instruction CALL"

  (test-encoding stream
    (serialize-instruction '(call 0) stream)
    #(#x10 0))

  (test-encoding stream
    (serialize-instruction '(call 10) stream)
    #(#x10 10))

  (test-encoding stream
    (serialize-instruction '(call 301) stream)
    #(#x10 #xAD #x02)))

(test instruction-call-errors
  "Test errors in serialization of instruction CALL"

  (test-error stream
    (serialize-instruction '(call -10) stream)
    type-error))

(test instruction-call_indirect
  "Test serialization of instruction CALL_INDIRECT"

  (test-encoding stream
    (serialize-instruction '(call_indirect 0) stream)
    #(#x11 0 #x00))

  (test-encoding stream
    (serialize-instruction '(call_indirect 10) stream)
    #(#x11 10 #x00))

  (test-encoding stream
    (serialize-instruction '(call_indirect 301) stream)
    #(#x11 #xAD #x02 #x00)))

(test instruction-call_indirect-errors
  "Test errors in serialization of instruction CALL_INDIRECT"

  (test-error stream
    (serialize-instruction '(call_indirect -10) stream)
    type-error))

;;;; Structured Blocks

(test instruction-block-no-result-type
  "Test serialization of BLOCK with no result type"

  (test-encoding stream
    (serialize-instruction
     '(block
       nop
       unreachable
       (br 1))
     stream)

    #(#x02 #x40
      #x01
      #x00
      #x0C 1
      #x0B)))

(test instruction-block-with-result-type
  "Test serialization of BLOCK with result type"

  (test-encoding stream
    (serialize-instruction
     '(block (result i32)
       nop
       (br 1)
       unreachable)
     stream)

    #(#x02 #x7F
      #x01
      #x0C 1
      #x00
      #x0B)))

(test instruction-loop-no-result-type
  "Test serialization of LOOP with no result type"

  (test-encoding stream
    (serialize-instruction
     '(loop
	 nop
	 unreachable
	   (br 1))
     stream)

    #(#x03 #x40
      #x01
      #x00
      #x0C 1
      #x0B)))

(test instruction-loop-with-result-type
  "Test serialization of LOOP with result type"

  (test-encoding stream
    (serialize-instruction
     '(loop (result i32)
	 nop
	 (br 1)
	 unreachable)
     stream)

    #(#x03 #x7F
      #x01
      #x0C 1
      #x00
      #x0B)))

(test instruction-if-no-else-no-result-type
  "Test serialization of IF without else branch without result type"

  (test-encoding stream
    (serialize-instruction
     '(if
       (then
	nop
	unreachable
	(br 1)))
     stream)

    #(#x04 #x40
      #x01
      #x00
      #x0C 1
      #x0B)))

(test instruction-if-no-else-with-result-type
  "Test serialization of IF without else branch with result type"

  (test-encoding stream
    (serialize-instruction
     '(if (result i32)
       (then
	nop
	(br 1)
	unreachable))
     stream)

    #(#x04 #x7F
      #x01
      #x0C 1
      #x00
      #x0B)))

(test instruction-if-else-no-result-type
  "Test serialization of IF with else branch without result type"

  (test-encoding stream
    (serialize-instruction
     '(if
       (then
	nop
	unreachable
	(br 1))

       (else
	(br 2)
	nop))
     stream)

    #(#x04 #x40
      #x01
      #x00
      #x0C 1

      #x05
      #x0C 2
      #x01

      #x0B)))

(test instruction-if-else-no-result-type
  "Test serialization of IF with else branch with result type"

  (test-encoding stream
    (serialize-instruction
     '(if (result i32)
       (then
	nop
	(br 1)
	unreachable)

       (else
	(br 2)
	nop))
     stream)

    #(#x04 #x7F
      #x01
      #x0C 1
      #x00

      #x05
      #x0C 2
      #x01

      #x0B)))

;;;; Parametric Instructions

(test instruction-drop
  "Test serialization of DROP"

  (test-encoding stream
    (serialize-instruction 'drop stream)
    #(#x1A)))

(test instruction-select
  "Test serialization of SELECT"

  (test-encoding stream
    (serialize-instruction 'select stream)
    #(#x1B)))

;;;; Variable Instructions

(test instruction-local.get
  "Test serialization of LOCAL.GET"

  (test-encoding stream
    (serialize-instruction '(local.get 5) stream)
    #(#x20 #x05))

  (test-encoding stream
    (serialize-instruction '(local.get 301) stream)
    #(#x20 #xAD #x02))

  (test-error stream
    (serialize-instruction '(local.get -5) stream)
    type-error))

(test instruction-local.set
  "Test serialization of LOCAL.SET"

  (test-encoding stream
    (serialize-instruction '(local.set 5) stream)
    #(#x21 #x05))

  (test-encoding stream
    (serialize-instruction '(local.set 301) stream)
    #(#x21 #xAD #x02))

  (test-error stream
    (serialize-instruction '(local.set -5) stream)
    type-error))

(test instruction-local.tee
  "Test serialization of LOCAL.TEE"

  (test-encoding stream
    (serialize-instruction '(local.tee 5) stream)
    #(#x22 #x05))

  (test-encoding stream
    (serialize-instruction '(local.tee 301) stream)
    #(#x22 #xAD #x02))

  (test-error stream
    (serialize-instruction '(local.tee -5) stream)
    type-error))

(test instruction-global.get
  "Test serialization of GLOBAL.GET"

  (test-encoding stream
    (serialize-instruction '(global.get 5) stream)
    #(#x23 #x05))

  (test-encoding stream
    (serialize-instruction '(global.get 301) stream)
    #(#x23 #xAD #x02))

  (test-error stream
    (serialize-instruction '(global.get -5) stream)
    type-error))

(test instruction-global.set
  "Test serialization of GLOBAL.SET"

  (test-encoding stream
    (serialize-instruction '(global.set 5) stream)
    #(#x24 #x05))

  (test-encoding stream
    (serialize-instruction '(global.set 301) stream)
    #(#x24 #xAD #x02))

  (test-error stream
    (serialize-instruction '(global.set -5) stream)
    type-error))

;;;; Load Instructions

(test instruction-i32.load-no-offset-alignment
  "Test serialization of I32.LOAD without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.load stream)

    #(#x28 #x02 #x00)))

(test instruction-i32.load-with-offset
  "Test serialization of I32.LOAD with offset"

  (test-encoding stream
    (serialize-instruction '(i32.load (offset 16)) stream)

    #(#x28 #x02 #x10)))

(test instruction-i32.load-alignment
  "Test serialization of I32.LOAD with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load (align 1)) stream)

    #(#x28 #x01 #x00)))

(test instruction-i32.load-offset-alignment
  "Test serialization of I32.LOAD with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load (offset 5) (align 1)) stream)

    #(#x28 #x01 #x05)))

(test instruction-i32.load-errors
  "Test errors in serialization of I32.LOAD"

  (test-error stream
    (serialize-instruction '(i32.load (offset -1)) stream)
    type-error))

(test instruction-i64.load-no-offset-alignment
  "Test serialization of I64.LOAD without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load stream)

    #(#x29 #x02 #x00)))

(test instruction-i65.load-with-offset
  "Test serialization of I64.LOAD with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load (offset 16)) stream)

    #(#x29 #x02 #x10)))

(test instruction-i64.load-alignment
  "Test serialization of I64.LOAD with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load (align 1)) stream)

    #(#x29 #x01 #x00)))

(test instruction-i64.load-offset-alignment
  "Test serialization of I64.LOAD with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load (offset 5) (align 1)) stream)

    #(#x29 #x01 #x05)))

(test instruction-i64.load-errors
  "Test errors in serialization of I64.LOAD"

  (test-error stream
    (serialize-instruction '(i64.load (offset -1)) stream)
    type-error))

(test instruction-f32.load-no-offset-alignment
  "Test serialization of F32.LOAD without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'f32.load stream)

    #(#x2A #x02 #x00)))

(test instruction-f32.load-with-offset
  "Test serialization of F32.LOAD with offset"

  (test-encoding stream
    (serialize-instruction '(f32.load (offset 16)) stream)

    #(#x2A #x02 #x10)))

(test instruction-F32.load-alignment
  "Test serialization of F32.LOAD with alignment"

  (test-encoding stream
    (serialize-instruction '(f32.load (align 1)) stream)

    #(#x2A #x01 #x00)))

(test instruction-f32.load-offset-alignment
  "Test serialization of F32.LOAD with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(f32.load (offset 5) (align 1)) stream)

    #(#x2A #x01 #x05)))

(test instruction-f32.load-errors
  "Test errors in serialization of F32.LOAD"

  (test-error stream
    (serialize-instruction '(f32.load (offset -1)) stream)
    type-error))

(test instruction-f64.load-no-offset-alignment
  "Test serialization of F64.LOAD without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'f64.load stream)

    #(#x2B #x02 #x00)))

(test instruction-f64.load-with-offset
  "Test serialization of F64.LOAD with offset"

  (test-encoding stream
    (serialize-instruction '(f64.load (offset 16)) stream)

    #(#x2B #x02 #x10)))

(test instruction-F64.load-alignment
  "Test serialization of F64.LOAD with alignment"

  (test-encoding stream
    (serialize-instruction '(f64.load (align 1)) stream)

    #(#x2B #x01 #x00)))

(test instruction-f64.load-offset-alignment
  "Test serialization of F64.LOAD with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(f64.load (offset 5) (align 1)) stream)

    #(#x2B #x01 #x05)))

(test instruction-f64.load-errors
  "Test errors in serialization of F64.LOAD"

  (test-error stream
    (serialize-instruction '(f64.load (offset -1)) stream)
    type-error))

(test instruction-i32.load8_s-no-offset-alignment
  "Test serialization of I32.LOAD8_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.load8_s stream)

    #(#x2C #x02 #x00)))

(test instruction-i32.load8_s-with-offset
  "Test serialization of I32.LOAD8_S with offset"

  (test-encoding stream
    (serialize-instruction '(i32.load8_s (offset 16)) stream)

    #(#x2C #x02 #x10)))

(test instruction-i32.load8_s-alignment
  "Test serialization of I32.LOAD8_S with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load8_s (align 1)) stream)

    #(#x2C #x01 #x00)))

(test instruction-i32.load8_s-offset-alignment
  "Test serialization of I32.LOAD8_S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load8_s (offset 5) (align 1)) stream)

    #(#x2C #x01 #x05)))

(test instruction-i32.load8_s-errors
  "Test errors in serialization of I32.LOAD8_S"

  (test-error stream
    (serialize-instruction '(i32.load8_s (offset -1)) stream)
    type-error))

(test instruction-i32.load8_u-no-offset-alignment
  "Test serialization of I32.LOAD8_U without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.load8_u stream)

    #(#x2D #x02 #x00)))

(test instruction-i32.load8_u-with-offset
  "Test serialization of I32.LOAD8_U with offset"

  (test-encoding stream
    (serialize-instruction '(i32.load8_u (offset 16)) stream)

    #(#x2D #x02 #x10)))

(test instruction-i32.load8_u-alignment
  "Test serialization of I32.LOAD8_U with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load8_u (align 1)) stream)

    #(#x2D #x01 #x00)))

(test instruction-i32.load8_u-offset-alignment
  "Test serialization of I32.LOAD8_U with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load8_u (offset 5) (align 1)) stream)

    #(#x2D #x01 #x05)))

(test instruction-i32.load8_u-errors
  "Test errors in serialization of I32.LOAD8_U"

  (test-error stream
    (serialize-instruction '(i32.load8_u (offset -1)) stream)
    type-error))

(test instruction-i32.load16_s-no-offset-alignment
  "Test serialization of I32.LOAD16_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.load16_s stream)

    #(#x2E #x02 #x00)))

(test instruction-i32.load16_s-with-offset
  "Test serialization of I32.LOAD16_S with offset"

  (test-encoding stream
    (serialize-instruction '(i32.load16_s (offset 16)) stream)

    #(#x2E #x02 #x10)))

(test instruction-i32.load16_s-alignment
  "Test serialization of I32.LOAD16_S with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load16_s (align 1)) stream)

    #(#x2E #x01 #x00)))

(test instruction-i32.load16_s-offset-alignment
  "Test serialization of I32.LOAD16_S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load16_s (offset 5) (align 1)) stream)

    #(#x2E #x01 #x05)))

(test instruction-i32.load16_s-errors
  "Test errors in serialization of I32.LOAD16_S"

  (test-error stream
    (serialize-instruction '(i32.load16_s (offset -1)) stream)
    type-error))

(test instruction-i32.load16_u-no-offset-alignment
  "Test serialization of I32.LOAD16_U without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.load16_u stream)

    #(#x2F #x02 #x00)))

(test instruction-i32.load16_u-with-offset
  "Test serialization of I32.LOAD16_U with offset"

  (test-encoding stream
    (serialize-instruction '(i32.load16_u (offset 16)) stream)

    #(#x2F #x02 #x10)))

(test instruction-i32.load16_u-alignment
  "Test serialization of I32.LOAD16_U with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load16_u (align 1)) stream)

    #(#x2F #x01 #x00)))

(test instruction-i32.load16_u-offset-alignment
  "Test serialization of I32.LOAD16_U with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load16_u (offset 5) (align 1)) stream)

    #(#x2F #x01 #x05)))

(test instruction-i32.load16_u-errors
  "Test errors in serialization of I32.LOAD16_U"

  (test-error stream
    (serialize-instruction '(i32.load16_u (offset -1)) stream)
    type-error))

(test instruction-i64.load8_s-no-offset-alignment
  "Test serialization of I64.LOAD8_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load8_s stream)

    #(#x30 #x02 #x00)))

(test instruction-i64.load8_s-with-offset
  "Test serialization of I64.LOAD8_S with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load8_s (offset 16)) stream)

    #(#x30 #x02 #x10)))

(test instruction-i64.load8_s-alignment
  "Test serialization of I64.LOAD8_S with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load8_s (align 1)) stream)

    #(#x30 #x01 #x00)))

(test instruction-i64.load8_s-offset-alignment
  "Test serialization of I64.LOAD8_S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load8_s (offset 5) (align 1)) stream)

    #(#x30 #x01 #x05)))

(test instruction-i64.load8_s-errors
  "Test errors in serialization of I64.LOAD8_S"

  (test-error stream
    (serialize-instruction '(i64.load8_s (offset -1)) stream)
    type-error))

(test instruction-i64.load8_u-no-offset-alignment
  "Test serialization of I64.LOAD8_U without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load8_u stream)

    #(#x31 #x02 #x00)))

(test instruction-i64.load8_u-with-offset
  "Test serialization of I64.LOAD8_U with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load8_u (offset 16)) stream)

    #(#x31 #x02 #x10)))

(test instruction-i64.load8_u-alignment
  "Test serialization of I64.LOAD8_U with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load8_u (align 1)) stream)

    #(#x31 #x01 #x00)))

(test instruction-i64.load8_u-offset-alignment
  "Test serialization of I64.LOAD8_U with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load8_u (offset 5) (align 1)) stream)

    #(#x31 #x01 #x05)))

(test instruction-i64.load8_u-errors
  "Test errors in serialization of I64.LOAD8_U"

  (test-error stream
    (serialize-instruction '(i64.load8_u (offset -1)) stream)
    type-error))

(test instruction-i64.load16_s-no-offset-alignment
  "Test serialization of I64.LOAD16_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load16_s stream)

    #(#x32 #x02 #x00)))

(test instruction-i64.load16_s-with-offset
  "Test serialization of I64.LOAD16_S with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load16_s (offset 16)) stream)

    #(#x32 #x02 #x10)))

(test instruction-i64.load16_s-alignment
  "Test serialization of I64.LOAD16_S with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load16_s (align 1)) stream)

    #(#x32 #x01 #x00)))

(test instruction-i64.load16_s-offset-alignment
  "Test serialization of I64.LOAD16_S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load16_s (offset 5) (align 1)) stream)

    #(#x32 #x01 #x05)))

(test instruction-i64.load16_s-errors
  "Test errors in serialization of I64.LOAD16_S"

  (test-error stream
    (serialize-instruction '(i64.load16_s (offset -1)) stream)
    type-error))

(test instruction-i64.load16_u-no-offset-alignment
  "Test serialization of I64.LOAD16_U without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load16_u stream)

    #(#x33 #x02 #x00)))

(test instruction-i64.load16_u-with-offset
  "Test serialization of I64.LOAD16_U with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load16_u (offset 16)) stream)

    #(#x33 #x02 #x10)))

(test instruction-i64.load16_u-alignment
  "Test serialization of I64.LOAD16_U with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load16_u (align 1)) stream)

    #(#x33 #x01 #x00)))

(test instruction-i64.load16_u-offset-alignment
  "Test serialization of I64.LOAD16_U with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load16_u (offset 5) (align 1)) stream)

    #(#x33 #x01 #x05)))

(test instruction-i64.load16_u-errors
  "Test errors in serialization of I64.LOAD16_U"

  (test-error stream
    (serialize-instruction '(i64.load16_u (offset -1)) stream)
    type-error))

(test instruction-i64.load32_s-no-offset-alignment
  "Test serialization of I64.LOAD32_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load32_s stream)

    #(#x34 #x02 #x00)))

(test instruction-i64.load32_s-with-offset
  "Test serialization of I64.LOAD32_S with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load32_s (offset 16)) stream)

    #(#x34 #x02 #x10)))

(test instruction-i64.load32_s-alignment
  "Test serialization of I64.LOAD32_S with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load32_s (align 1)) stream)

    #(#x34 #x01 #x00)))

(test instruction-i64.load32_s-offset-alignment
  "Test serialization of I64.LOAD32_S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load32_s (offset 5) (align 1)) stream)

    #(#x34 #x01 #x05)))

(test instruction-i64.load32_s-errors
  "Test errors in serialization of I64.LOAD32_S"

  (test-error stream
    (serialize-instruction '(i64.load32_s (offset -1)) stream)
    type-error))

(test instruction-i64.load64_s-no-offset-alignment
  "Test serialization of I64.LOAD64_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load32_u stream)

    #(#x35 #x02 #x00)))

(test instruction-i64.load_64s-with-offset
  "Test serialization of I64.LOAD_64S with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load32_u (offset 16)) stream)

    #(#x35 #x02 #x10)))

(test instruction-i64.load_64s-alignment
  "Test serialization of I64.LOAD_64S with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load32_u (align 1)) stream)

    #(#x35 #x01 #x00)))

(test instruction-i64.load_64s-offset-alignment
  "Test serialization of I64.LOAD_64S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load32_u (offset 5) (align 1)) stream)

    #(#x35 #x01 #x05)))

(test instruction-i64.load_64s-errors
  "Test errors in serialization of I64.LOAD_64S"

  (test-error stream
    (serialize-instruction '(i64.load32_u (offset -1)) stream)
    type-error))

    ;;;; Store Instructions

(test instruction-i32.store-no-offset-alignment
  "Test serialization of I32.store without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.store stream)

    #(#x36 #x02 #x00)))

(test instruction-i32.store-with-offset
  "Test serialization of I32.store with offset"

  (test-encoding stream
    (serialize-instruction '(i32.store (offset 16)) stream)

    #(#x36 #x02 #x10)))

(test instruction-i32.store-alignment
  "Test serialization of I32.store with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store (align 1)) stream)

    #(#x36 #x01 #x00)))

(test instruction-i32.store-offset-alignment
  "Test serialization of I32.store with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store (offset 5) (align 1)) stream)

    #(#x36 #x01 #x05)))

(test instruction-i32.store-errors
  "Test errors in serialization of I32.store"

  (test-error stream
    (serialize-instruction '(i32.store (offset -1)) stream)
    type-error))

(test instruction-i64.store-no-offset-alignment
  "Test serialization of I64.store without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.store stream)

    #(#x37 #x02 #x00)))

(test instruction-i65.store-with-offset
  "Test serialization of I64.store with offset"

  (test-encoding stream
    (serialize-instruction '(i64.store (offset 16)) stream)

    #(#x37 #x02 #x10)))

(test instruction-i64.store-alignment
  "Test serialization of I64.store with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store (align 1)) stream)

    #(#x37 #x01 #x00)))

(test instruction-i64.store-offset-alignment
  "Test serialization of I64.store with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store (offset 5) (align 1)) stream)

    #(#x37 #x01 #x05)))

(test instruction-i64.store-errors
  "Test errors in serialization of I64.store"

  (test-error stream
    (serialize-instruction '(i64.store (offset -1)) stream)
    type-error))

(test instruction-f32.store-no-offset-alignment
  "Test serialization of F32.store without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'f32.store stream)

    #(#x38 #x02 #x00)))

(test instruction-f32.store-with-offset
  "Test serialization of F32.store with offset"

  (test-encoding stream
    (serialize-instruction '(f32.store (offset 16)) stream)

    #(#x38 #x02 #x10)))

(test instruction-F32.store-alignment
  "Test serialization of F32.store with alignment"

  (test-encoding stream
    (serialize-instruction '(f32.store (align 1)) stream)

    #(#x38 #x01 #x00)))

(test instruction-f32.store-offset-alignment
  "Test serialization of F32.store with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(f32.store (offset 5) (align 1)) stream)

    #(#x38 #x01 #x05)))

(test instruction-f32.store-errors
  "Test errors in serialization of F32.store"

  (test-error stream
    (serialize-instruction '(f32.store (offset -1)) stream)
    type-error))

(test instruction-f64.store-no-offset-alignment
  "Test serialization of F64.store without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'f64.store stream)

    #(#x39 #x02 #x00)))

(test instruction-f64.store-with-offset
  "Test serialization of F64.store with offset"

  (test-encoding stream
    (serialize-instruction '(f64.store (offset 16)) stream)

    #(#x39 #x02 #x10)))

(test instruction-F64.store-alignment
  "Test serialization of F64.store with alignment"

  (test-encoding stream
    (serialize-instruction '(f64.store (align 1)) stream)

    #(#x39 #x01 #x00)))

(test instruction-f64.store-offset-alignment
  "Test serialization of F64.store with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(f64.store (offset 5) (align 1)) stream)

    #(#x39 #x01 #x05)))

(test instruction-f64.store-errors
  "Test errors in serialization of F64.store"

  (test-error stream
    (serialize-instruction '(f64.store (offset -1)) stream)
    type-error))

(test instruction-i32.store8-no-offset-alignment
  "Test serialization of I32.store8 without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.store8 stream)

    #(#x3A #x02 #x00)))

(test instruction-i32.store8-with-offset
  "Test serialization of I32.store8 with offset"

  (test-encoding stream
    (serialize-instruction '(i32.store8 (offset 16)) stream)

    #(#x3A #x02 #x10)))

(test instruction-i32.store8-alignment
  "Test serialization of I32.store8 with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store8 (align 1)) stream)

    #(#x3A #x01 #x00)))

(test instruction-i32.store8-offset-alignment
  "Test serialization of I32.store8 with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store8 (offset 5) (align 1)) stream)

    #(#x3A #x01 #x05)))

(test instruction-i32.store8-errors
  "Test errors in serialization of I32.store8"

  (test-error stream
    (serialize-instruction '(i32.store8 (offset -1)) stream)
    type-error))

(test instruction-i32.store16-no-offset-alignment
  "Test serialization of I32.store16 without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.store16 stream)

    #(#x3B #x02 #x00)))

(test instruction-i32.store16-with-offset
  "Test serialization of I32.store16 with offset"

  (test-encoding stream
    (serialize-instruction '(i32.store16 (offset 16)) stream)

    #(#x3B #x02 #x10)))

(test instruction-i32.store16-alignment
  "Test serialization of I32.store16 with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store16 (align 1)) stream)

    #(#x3B #x01 #x00)))

(test instruction-i32.store16-offset-alignment
  "Test serialization of I32.store16 with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store16 (offset 5) (align 1)) stream)

    #(#x3B #x01 #x05)))

(test instruction-i32.load16_s-errors
  "Test errors in serialization of I32.LOAD16_S"

  (test-error stream
    (serialize-instruction '(i32.store16 (offset -1)) stream)
    type-error))

(test instruction-i64.store8-no-offset-alignment
  "Test serialization of I64.store8 without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.store8 stream)

    #(#x3C #x02 #x00)))

(test instruction-i64.store8-with-offset
  "Test serialization of I64.store8 with offset"

  (test-encoding stream
    (serialize-instruction '(i64.store8 (offset 16)) stream)

    #(#x3C #x02 #x10)))

(test instruction-i64.store8-alignment
  "Test serialization of I64.store8 with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store8 (align 1)) stream)

    #(#x3C #x01 #x00)))

(test instruction-i64.store8-offset-alignment
  "Test serialization of I64.store8 with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store8 (offset 5) (align 1)) stream)

    #(#x3C #x01 #x05)))

(test instruction-i64.store8-errors
  "Test errors in serialization of I64.store8"

  (test-error stream
    (serialize-instruction '(i64.store8 (offset -1)) stream)
    type-error))

(test instruction-i64.store16-no-offset-alignment
  "Test serialization of I64.store16 without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.store16 stream)

    #(#x3D #x02 #x00)))

(test instruction-i64.store16-with-offset
  "Test serialization of I64.store16 with offset"

  (test-encoding stream
    (serialize-instruction '(i64.store16 (offset 16)) stream)

    #(#x3D #x02 #x10)))

(test instruction-i64.store16-alignment
  "Test serialization of I64.store16 with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store16 (align 1)) stream)

    #(#x3D #x01 #x00)))

(test instruction-i64.store16-offset-alignment
  "Test serialization of I64.store16 with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store16 (offset 5) (align 1)) stream)

    #(#x3D #x01 #x05)))

(test instruction-i64.store16-errors
  "Test errors in serialization of I64.store16"

  (test-error stream
    (serialize-instruction '(i64.store16 (offset -1)) stream)
    type-error))

(test instruction-i64.store32-no-offset-alignment
  "Test serialization of I64.store32 without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.store32 stream)

    #(#x3E #x02 #x00)))

(test instruction-i64.store32-with-offset
  "Test serialization of I64.store32 with offset"

  (test-encoding stream
    (serialize-instruction '(i64.store32 (offset 16)) stream)

    #(#x3E #x02 #x10)))

(test instruction-i64.store32-alignment
  "Test serialization of I64.store32 with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store32 (align 1)) stream)

    #(#x3E #x01 #x00)))

(test instruction-i64.store32-offset-alignment
  "Test serialization of I64.store32 with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store32 (offset 5) (align 1)) stream)

    #(#x3E #x01 #x05)))

(test instruction-i64.store32-errors
  "Test errors in serialization of I64.store32"

  (test-error stream
    (serialize-instruction '(i64.store32 (offset -1)) stream)
    type-error))

    ;;;; Memory Instructions

(test instruction-memory.size
  "Test serialization of memory.size"

  (test-encoding stream
    (serialize-instruction 'memory.size stream)
    #(#x3F #x00)))

(test instruction-memory.grow
  "Test serialization of memory.grow"

  (test-encoding stream
    (serialize-instruction 'memory.grow stream)
    #(#x40 #x00)))

  ;;;; Constant Instructions

(test instruction-i32.const
  "Test serialization of I32.CONST"

  (test-encoding stream
    (serialize-instruction '(i32.const 1) stream)
    #(#x41 1))

  (test-encoding stream
    (serialize-instruction '(i32.const -5) stream)
    #(#x41 #x7B))

  (test-encoding stream
    (serialize-instruction '(i32.const 350) stream)

    #(#x41 #xDE #x02))

  (test-encoding stream
    (serialize-instruction '(i32.const -350) stream)

    #(#x41 #xA2 #x7D))

  (test-encoding stream
    (serialize-instruction `(i32.const ,(1- (expt 2 32))) stream)

    #(#x41 #x7F))

  (test-encoding stream
    (serialize-instruction `(i32.const ,(expt 2 31)) stream)

    #(#x41 #x80 #x80 #x80 #x80 #x78))

  (test-encoding stream
    (serialize-instruction `(i32.const ,(+ (expt 2 31) (expt 2 23) 4 1)) stream)

    #(#x41 #x85 #x80 #x80 #x84 #x78)))

(test instruction-i64.const
  "Test serialization of I64.CONST"

  (test-encoding stream
    (serialize-instruction '(i64.const 1) stream)
    #(#x42 1))

  (test-encoding stream
    (serialize-instruction '(i64.const -5) stream)
    #(#x42 #x7B))

  (test-encoding stream
    (serialize-instruction '(i64.const 350) stream)

    #(#x42 #xDE #x02))

  (test-encoding stream
    (serialize-instruction '(i64.const -350) stream)

    #(#x42 #xA2 #x7D))

  (test-encoding stream
    (serialize-instruction `(i64.const ,(1- (expt 2 64))) stream)

    #(#x42 #x7F))

  (test-encoding stream
    (serialize-instruction `(i64.const ,(expt 2 63)) stream)

    #(#x42 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x80 #x7F))

  (test-encoding stream
    (serialize-instruction `(i64.const ,(+ (expt 2 63) (expt 2 43) (expt 2 32) 16 1)) stream)

    #(#x42 #x91 #x80 #x80 #x80 #x90 #x80 #x82 #x80 #x80 #x7F)))

(test instruction-f32.const
  "Test serialization of F32.CONST"

  (test-encoding stream
    (serialize-instruction '(f32.const 1.5) stream)

    #(#x43 #x00 #x00 #xC0 #x3F)))

(test instruction-f64.const
  "Test serialization of F64.CONST"

  (test-encoding stream
    (serialize-instruction '(f64.const 1.5) stream)

    #(#x44 #x00 #x00 #x00 #x00 #x00 #x00 #xF8 #x3F)))

  ;;;; Arithmetic Instructions

(test instruction-arithmetic
  "Test serialization of arithmetic instructions"

  (test-encode-instruction i32.eqz #x45)
  (test-encode-instruction i32.eq #x46)
  (test-encode-instruction i32.ne #x47)
  (test-encode-instruction i32.lt_s #x48)
  (test-encode-instruction i32.lt_u #x49)
  (test-encode-instruction i32.gt_s #x4A)
  (test-encode-instruction i32.gt_u #x4B)
  (test-encode-instruction i32.le_s #x4C)
  (test-encode-instruction i32.le_u #x4D)
  (test-encode-instruction i32.ge_s #x4E)
  (test-encode-instruction i32.ge_u #x4F)

  (test-encode-instruction i64.eqz #x50)
  (test-encode-instruction i64.eq #x51)
  (test-encode-instruction i64.ne #x52)
  (test-encode-instruction i64.lt_s #x53)
  (test-encode-instruction i64.lt_u #x54)
  (test-encode-instruction i64.gt_s #x55)
  (test-encode-instruction i64.gt_u #x56)
  (test-encode-instruction i64.le_s #x57)
  (test-encode-instruction i64.le_u #x58)
  (test-encode-instruction i64.ge_s #x59)
  (test-encode-instruction i64.ge_u #x5A)

  (test-encode-instruction f32.eq #x5B)
  (test-encode-instruction f32.ne #x5C)
  (test-encode-instruction f32.lt #x5D)
  (test-encode-instruction f32.gt #x5E)
  (test-encode-instruction f32.le #x5F)
  (test-encode-instruction f32.ge #x60)

  (test-encode-instruction f64.eq #x61)
  (test-encode-instruction f64.ne #x62)
  (test-encode-instruction f64.lt #x63)
  (test-encode-instruction f64.gt #x64)
  (test-encode-instruction f64.le #x65)
  (test-encode-instruction f64.ge #x66)

  (test-encode-instruction i32.clz #x67)
  (test-encode-instruction i32.ctz #x68)
  (test-encode-instruction i32.popcnt #x69)
  (test-encode-instruction i32.add #x6A)
  (test-encode-instruction i32.sub #x6B)
  (test-encode-instruction i32.mul #x6C)
  (test-encode-instruction i32.div_s #x6D)
  (test-encode-instruction i32.div_u #x6E)
  (test-encode-instruction i32.rem_s #x6F)
  (test-encode-instruction i32.rem_u #x70)
  (test-encode-instruction i32.and #x71)
  (test-encode-instruction i32.or #x72)
  (test-encode-instruction i32.xor #x73)
  (test-encode-instruction i32.shl #x74)
  (test-encode-instruction i32.shr_s #x75)
  (test-encode-instruction i32.shr_u #x76)
  (test-encode-instruction i32.rotl #x77)
  (test-encode-instruction i32.rotr #x78)

  (test-encode-instruction i64.clz #x79)
  (test-encode-instruction i64.ctz #x7A)
  (test-encode-instruction i64.popcnt #x7B)
  (test-encode-instruction i64.add #x7C)
  (test-encode-instruction i64.sub #x7D)
  (test-encode-instruction i64.mul #x7E)
  (test-encode-instruction i64.div_s #x7F)
  (test-encode-instruction i64.div_u #x80)
  (test-encode-instruction i64.rem_s #x81)
  (test-encode-instruction i64.rem_u #x82)
  (test-encode-instruction i64.and #x83)
  (test-encode-instruction i64.or #x84)
  (test-encode-instruction i64.xor #x85)
  (test-encode-instruction i64.shl #x86)
  (test-encode-instruction i64.shr_s #x87)
  (test-encode-instruction i64.shr_u #x88)
  (test-encode-instruction i64.rotl #x89)
  (test-encode-instruction i64.rotr #x8A)

  (test-encode-instruction f32.abs #x8B)
  (test-encode-instruction f32.neg #x8C)
  (test-encode-instruction f32.ceil #x8D)
  (test-encode-instruction f32.floor #x8E)
  (test-encode-instruction f32.trunc #x8F)
  (test-encode-instruction f32.nearest #x90)
  (test-encode-instruction f32.sqrt #x91)
  (test-encode-instruction f32.add #x92)
  (test-encode-instruction f32.sub #x93)
  (test-encode-instruction f32.mul #x94)
  (test-encode-instruction f32.div #x95)
  (test-encode-instruction f32.min #x96)
  (test-encode-instruction f32.max #x97)
  (test-encode-instruction f32.copysign #x98)

  (test-encode-instruction f64.abs #x99)
  (test-encode-instruction f64.neg #x9A)
  (test-encode-instruction f64.ceil #x9B)
  (test-encode-instruction f64.trunc #x9C)
  (test-encode-instruction f64.floor #x9D)
  (test-encode-instruction f64.nearest #x9E)
  (test-encode-instruction f64.sqrt #x9F)
  (test-encode-instruction f64.add #xA0)
  (test-encode-instruction f64.sub #xA1)
  (test-encode-instruction f64.mul #xA2)
  (test-encode-instruction f64.div #xA3)
  (test-encode-instruction f64.min #xA4)
  (test-encode-instruction f64.max #xA5)
  (test-encode-instruction f64.copysign #xA6)

  (test-encode-instruction i32.wrap_i64 #xA7)
  (test-encode-instruction i32.trunc_f32_s #xA8)
  (test-encode-instruction i32.trunc_f32_u #xA9)
  (test-encode-instruction i32.trunc_f64_s #xAA)
  (test-encode-instruction i32.trunc_f64_u #xAB)
  (test-encode-instruction i64.extend_i32_s #xAC)
  (test-encode-instruction i64.extend_i32_u #xAD)
  (test-encode-instruction i64.trunc_f32_s #xAE)
  (test-encode-instruction i64.trunc_f32_u #xAF)
  (test-encode-instruction i64.trunc_f64_s #xB0)
  (test-encode-instruction i64.trunc_f64_u #xB1)

  (test-encode-instruction f32.convert_i32_s #xB2)
  (test-encode-instruction f32.convert_i32_u #xB3)
  (test-encode-instruction f32.convert_i64_s #xB4)
  (test-encode-instruction f32.convert_i64_u #xB5)
  (test-encode-instruction f32.demote_f64 #xB6)

  (test-encode-instruction f64.convert_i32_s #xB7)
  (test-encode-instruction f64.convert_i32_u #xB8)
  (test-encode-instruction f64.convert_i64_s #xB9)
  (test-encode-instruction f64.convert_i64_u #xBA)
  (test-encode-instruction f64.promote_f64 #xBB)

  (test-encode-instruction i32.reinterpret_f32 #xBC)
  (test-encode-instruction i64.reinterpret_f64 #xBD)
  (test-encode-instruction f32.reinterpret_i32 #xBE)
  (test-encode-instruction f64.reinterpret_i64 #xBF))


;;;; Test Serialization of Expressions

(test serialize-expressions
  "Test serialization of expressions"

  (test-encoding stream
    (serialize-expression
     '((i32.const 1)
       (i32.const 2)
       i32.add)
     stream)

    #(#x41 1
      #x41 2
      #x6A
      #x0B)))


;;;; Test Serialization of Sections

(test section-type
  "Test serialization of type sections"

  (test-encoding stream
    (serialize-types
     (list (make-wasm-function-type :params '(i32 i32) :results '(i32))
	   (make-wasm-function-type :results '(i32)))

     stream)

    #(#x01 #x0B #x02
      #x60 #x02 #x7F #x7F #x01 #x7F
      #x60 #x00 #x01 #x7F)))

(test section-imports
  "Test serialization of imports sections"

  (test-encoding stream
    (serialize-imports
     (list (make-wasm-import :module "runtime"
			     :name "fn"
			     :type :func
			     :desc 5)

	   (make-wasm-import :module "env"
			     :name "table"
			     :type :table
			     :desc (make-wasm-table-type :min 2))

	   (make-wasm-import :module "env"
			     :name "memory"
			     :type :memory
			     :desc (make-wasm-limit :min 1))

	   (make-wasm-import :module "env"
			     :name "stack"
			     :type :global
			     :desc '(i32 t)))
     stream)

    #(#x02 #x37				; Section ID 2, 54 bytes
      #x04				; 4 Imports

      ;; Import 1: runtime.fn
      #x07 #x72 #x75 #x6E #x74 #x69 #x6D #x65 ; "runtime"
      #x02 #x66 #x6E			      ; "fn"
      #x00 #x05				      ; Function type 5

      ;; Import 2: env.table
      #x03 #x65 #x6E #x76		; "env"
      #x05 #x74 #x61 #x62 #x6C #x65	; "table"
      #x01 #x70 #x00 #x02		; Table type funcref, min: 0

      ;; Import 3: env.memory
      #x03 #x65 #x6E #x76		 ; "env"
      #x06 #x6D #x65 #x6D #x6F #x72 #x79 ; "memory"
      #x02 #x00 #x01			 ; Memory type, min: 0

      ;; Import 4: env.stack
      #x03 #x65 #x6E #x76		; "env"
      #x05 #x73 #x74 #x61 #x63 #x6B	; "stack"
      #x03 #x7F #x01)))		; Global type i32 mutable

(test section-function
  "Test serialization of function sections"

  (test-encoding stream
    (serialize-function-types '(5 1 3) stream)

    #(#x03 #x04			; Section ID 3, 4 bytes
      #x03 #x05 #x01 #x03)))	; 3 Type Indices: [5, 1, 3]

(test section-table
  "Test serialization of table section"

  (test-encoding stream
    (serialize-table-types
     (list (make-wasm-table-type :min 2 :max 100))
     stream)

    #(#x04 #x05		  ; Section ID 4, 5 bytes
      #x01 #x70 #x01 2 100))) ; 1 Table: funcref, min: 2, max: 100

(test section-memory
  "Test serialization of memory section"

  (test-encoding stream
    (serialize-memory-types
     (list (make-wasm-limit :min 3))
     stream)

    #(#x05 #x03			; Section ID 5, 3 bytes
      #x01 #x00 3)))		; 1 Memory, min: 3

(test section-global
  "Test serialization of globals section"

  (test-encoding stream
    (serialize-global-section
     (list (make-wasm-global :type 'i32 :init '((i32.const 3)))
	   (make-wasm-global :type 'i64 :mutable-p t))
     stream)

    #(#x06	#x09			; Section ID 6, 9 bytes
      #x02				; 2 Globals

      #x7F #x00 #x41 3 #x0B		; I32 Immutable Global
      #x7E #x01 #x0B)))		; I64 Mutable Global

(test section-exports
  "Test serialization of exports section"

  (test-encoding stream
    (serialize-export-section
     (list (make-wasm-export :name "f1" :type :func :index 2)
	   (make-wasm-export :name "tab" :type :table :index 0)
	   (make-wasm-export :name "mem" :type :memory :index 0)
	   (make-wasm-export :name "stack" :type :global :index 6))
     stream)

    #(#x07 #x1A			; Section ID 7, 26 bytes
      #x04				; 4 Exports

      ;; f1
      #x02 #x66 #x31		; name "f1"
      #x00 #x02			; Function 2

      ;; tab
      #x03 #x74 #x61 #x62		; name "tab"
      #x01 #x00			; Table 0

      ;; mem
      #x03 #x6D #x65 #x6D		; name "tab"
      #x02 #x00			; Memory 0

      ;; stack
      #x05 #x73 #x74 #x61 #x63 #x6B ; name "stack"
      #x03 #x06)))			; Global 6

(test section-start
  "Test serialization of start section"

  (test-encoding stream
    (serialize-start-section 5 stream)

    #(#x08 #x01 #x05)))

(test section-element
  "Test serialization of table elements"

  (test-encoding stream
    (serialize-table-elements
     (list (make-wasm-table
	    :index 0
	    :offset '((i32.const 2))
	    :init
	    (make-wasm-table-init-index
	     :functions '(5 1 3)))

	   (make-wasm-table
	    :index 0
	    :offset '((i32.const 100))
	    :init
	    (make-wasm-table-init-index
	     :functions '(9 8))))
     stream)

    #(#x09 #x11		   ; Section ID 9, 16 Bytes
      #x02			   ; Two Table Element Initializations

      #x00				; Table 0
      #x41 #x02 #x0B		; i32.const 2
      #x03 5 1 3			; 3 Functions [5, 1, 3]

      #x00				; Table 0
      #x41 #xE4 #x0 #x0B			; i32.const 100
      #x02 9 8)))			; 2 Functions [5, 1, 3]

(test section-code
  "Test serialization of code section"

  (test-encoding stream
    (serialize-functions
     (list (make-wasm-function :type 0
			       :code
			       '((local.get 0)
				 (local.get 1)
				 i32.add))

	   (make-wasm-function :type 1
			       :locals '(i32 i32 f32 i64 i64 i64)
			       :code
			       '((local.get 0)
				 (local.set 2)

				 (local.get 1)
				 (local.tee 3))))
     stream)

    #(#x0A #x1A			; Section ID 10
      #x02				; 2 Functions

      #x07				; Code size = 7 bytes
      #x00				; No Locals
      #x20 #x00
      #x20 #x01
      #x6A
      #x0B

      #x10				     ; Code Size = 16 Bytes
      #x03 #x02 #x7F #x01 #x7D #x03 #x7E ; 3 Locals (2 x i32) (1 x f32) (3 x i64)
      #x20 #x00
      #x21 #x02
      #x20 #x01
      #x22 #x03
      #x0B)))

(test section-data
  "Test serialization of data section"

  (test-encoding stream
    (serialize-data-section
     (list (make-wasm-data :offset '((i32.const 16))
			   :bytes #(1 5 9 45)
			   :memory 0)
	   (make-wasm-data :offset '((i32.const 0))
			   :bytes #(7 8 9)
			   :memory 0))
     stream)

    #(#x0B #x12			; Section ID 11, 18 bytes
      #x02				; 2 Data Segments

      #x00
      #x41 #x10 #x0B		; i32.const 16
      #x04 1 5 9 45

      #x00
      #x41 #x0 #x0B			; i32.const 0
      #x03 7 8 9)))
