;;; instructions.lisp
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

(defpackage wasm-encoder/test.instructions
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

(in-package #:wasm-encoder/test.instructions)


;;; Test Suite Definition

(def-suite instructions
    :description "Test encoding of instructions"
    :in wasm-encoder)

(in-suite instructions)


;;; Test Control Instructions

(test unreachable
  "Test serialization of UNREACHABLE"

  (test-encoding stream
    (serialize-instruction 'unreachable stream)
    #(#x00)))

(test nop
  "Test serialization of NOP"

  (test-encoding stream
    (serialize-instruction 'nop stream)
    #(#x01)))

(test return
  "Test serialization of RETURN"

  (test-encoding stream
    (serialize-instruction 'return stream)
    #(#x0F)))


;;; Test Branch Instructions

(test br
  "Test serialization of BR"

  (test-encoding stream
    (serialize-instruction '(br 0) stream)
    #(#x0C 0))

  (test-encoding stream
    (serialize-instruction '(br 10) stream)
    #(#x0C 10))

  (test-encoding stream
    (serialize-instruction '(br 300) stream)
    #(#x0C #xAC #x02)))

(test br-errors
  "Test errors in serialization of BR"

  (test-error stream
    (serialize-instruction '(br -10) stream)
    type-error)

  (test-error stream
    (serialize-instruction 'br stream)
    error))

(test br_if
  "Test serialization of BR_IF"

  (test-encoding stream
    (serialize-instruction '(br_if 0) stream)
    #(#x0D 0))

  (test-encoding stream
    (serialize-instruction '(br_if 10) stream)
    #(#x0D 10))

  (test-encoding stream
    (serialize-instruction '(br_if 300) stream)
    #(#x0D #xAC #x02)))

(test br_if-errors
  "Test errors in serialization of BR_IF"

  (test-error stream
    (serialize-instruction '(br_if -10) stream)
    type-error)

  (test-error stream
    (serialize-instruction 'br_if stream)
    error))

(test br_table
  "Test serialization of BR_TABLE"

  (test-encoding stream
    (serialize-instruction '(br_table 1 0 5) stream)

    #(#x0E #x02 1 0 5))

  (test-encoding stream
    (serialize-instruction '(br_table 1 5 300 0 4) stream)

    #(#x0E #x04 1 5 #xAC #x02 0 4)))

(test br_table-errors
  "Test errors in serialization of BR_TABLE"

  (test-error stream
    (serialize-instruction '(br_table 1 6 -3 4) stream)
    type-error)

  (test-error stream
    (serialize-instruction 'br_table stream)
    error))


;;; Test Call Instructions

(test call
  "Test serialization of CALL"

  (test-encoding stream
    (serialize-instruction '(call 0) stream)
    #(#x10 0))

  (test-encoding stream
    (serialize-instruction '(call 10) stream)
    #(#x10 10))

  (test-encoding stream
    (serialize-instruction '(call 301) stream)
    #(#x10 #xAD #x02)))

(test call-errors
  "Test errors in serialization of CALL"

  (test-error stream
    (serialize-instruction '(call -10) stream)
    type-error)

  (test-error stream
    (serialize-instruction 'call stream)
    error))

(test call_indirect
  "Test serialization of CALL_INDIRECT"

  (test-encoding stream
    (serialize-instruction '(call_indirect 0) stream)
    #(#x11 0 #x00))

  (test-encoding stream
    (serialize-instruction '(call_indirect 10) stream)
    #(#x11 10 #x00))

  (test-encoding stream
    (serialize-instruction '(call_indirect 301) stream)
    #(#x11 #xAD #x02 #x00)))

(test call_indirect-errors
  "Test errors in serialization of CALL_INDIRECT"

  (test-error stream
    (serialize-instruction '(call_indirect -10) stream)
    type-error)

  (test-error stream
    (serialize-instruction 'call_indirect stream)
    error))


;;; Test Structured Blocks

(test block-no-result-type
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

(test block-with-result-type
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

(test block-with-type-index
  "Test serialization of BLOCK with function type index"

  (test-encoding stream
    (serialize-instruction
     '(block (type #x77)
       nop
       (br 1)
       unreachable)
     stream)

    #(#x02 #xF7 #x00
      #x01
      #x0C 1
      #x00
      #x0B)))

(test loop-no-result-type
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

(test loop-with-result-type
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

(test loop-with-type-index
  "Test serialization of LOOP with function type index"

  (test-encoding stream
    (serialize-instruction
     '(loop (type #x35)
	 nop
	 (br 1)
	 unreachable)
     stream)

    #(#x03 #x35
      #x01
      #x0C 1
      #x00
      #x0B)))

(test if-no-else-no-result-type
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

(test if-no-else-with-result-type
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

(test if-no-else-with-type-index
  "Test serialization of IF without else branch with function type index"

  (test-encoding stream
    (serialize-instruction
     '(if (type #x3F2)
       (then
	nop
	(br 1)
	unreachable))
     stream)

    #(#x04 #xF2 #x07
      #x01
      #x0C 1
      #x00
      #x0B)))

(test if-else-no-result-type
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

(test if-else-with-result-type
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

(test if-else-with-type-index
  "Test serialization of IF with else branch with function type index"

  (test-encoding stream
    (serialize-instruction
     '(if (type #x40)
       (then
	nop
	(br 1)
	unreachable)

       (else
	(br 2)
	nop))
     stream)

    #(#x04 #xC0 #x0
      #x01
      #x0C 1
      #x00

      #x05
      #x0C 2
      #x01

      #x0B)))


;;; Test Reference Instructions

(test ref.null
  "Test serialization of REF.NULL"

  (test-encoding stream
    (serialize-instruction '(ref.null funcref) stream)
    #(#xD0 #x70))

  (test-encoding stream
    (serialize-instruction '(ref.null externref) stream)
    #(#xD0 #x6F)))

(test ref.is_null
  "Test serialization of REF.IS_NULL"

  (test-encoding stream
    (serialize-instruction 'ref.is_null stream)
    #(#xD1)))

(test ref.func
  "Test serialization of REF.FUNC"

  (test-encoding stream
    (serialize-instruction '(ref.func #xF7) stream)
    #(#xD2 #xF7 #x01)))


;;; Test Parametric Instructions

(test drop
  "Test serialization of DROP"

  (test-encoding stream
    (serialize-instruction 'drop stream)
    #(#x1A)))

(test select
  "Test serialization of SELECT"

  (test-encoding stream
    (serialize-instruction 'select stream)
    #(#x1B)))

(test multiple-select
  "Test serialization of SELECT with multiple values."

  ;; Currently this is not semantically valid WASM but the encoded is
  ;; valid and may be come semantically valid in a future version of
  ;; the spec.

  (test-encoding stream
    (serialize-instruction '(select i64 i32) stream)
    #(#x1C #x02 #x7E #x7F)))


;;; Test Variable Instructions

(test local.get
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

(test local.set
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

(test local.tee
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

(test global.get
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

(test global.set
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


;;; Test Table Instructions

(test table.get
  "Test serialization of TABLE.GET"

  (test-encoding stream
    (serialize-instruction '(table.get 0) stream)

    #(#x25 #x00))

  (test-encoding stream
    (serialize-instruction '(table.get #xE5) stream)

    #(#x25 #xE5 #x01)))

(test table.set
  "Test serialization of TABLE.SET"

  (test-encoding stream
    (serialize-instruction '(table.set 0) stream)

    #(#x26 #x00))

  (test-encoding stream
    (serialize-instruction '(table.set #xE5) stream)

    #(#x26 #xE5 #x01)))

(test table.init
  "Test serialization of TABLE.INIT"

  (test-encoding stream
    (serialize-instruction '(table.init 0 #xC7) stream)

    #(#xFC #x0C #xC7 #x01 #x00))

  (test-encoding stream
    (serialize-instruction '(table.init #xC5 #x34) stream)

    #(#xFC #x0C #x34 #xC5 #x01)))

(test elem.drop
  "Test serialization of ELEM.DROP"

  (test-encoding stream
    (serialize-instruction '(elem.drop 0) stream)

    #(#xFC #x0D #x00))

  (test-encoding stream
    (serialize-instruction '(elem.drop #xE5) stream)

    #(#xFC #x0D #xE5 #x01)))

(test table.copy
  "Test serialization of TABLE.COPY"

  (test-encoding stream
    (serialize-instruction '(table.copy 0 1) stream)

    #(#xFC #x0E #x00 #x01))

  (test-encoding stream
    (serialize-instruction '(table.copy #xC5 #xE4) stream)

    #(#xFC #x0E #xC5 #x01 #xE4 #x01)))

(test table.grow
  "Test serialization of TABLE.GROW"

  (test-encoding stream
    (serialize-instruction '(table.grow 0) stream)

    #(#xFC #x0F #x00))

  (test-encoding stream
    (serialize-instruction '(table.grow #xE5) stream)

    #(#xFC #x0F #xE5 #x01)))

(test table.size
  "Test serialization of TABLE.SIZE"

  (test-encoding stream
    (serialize-instruction '(table.size 0) stream)

    #(#xFC #x10 #x00))

  (test-encoding stream
    (serialize-instruction '(table.size #xE5) stream)

    #(#xFC #x10 #xE5 #x01)))

(test table.fill
  "Test serialization of TABLE.FILL"

  (test-encoding stream
    (serialize-instruction '(table.fill 0) stream)

    #(#xFC #x11 #x00))

  (test-encoding stream
    (serialize-instruction '(table.fill #xC3) stream)

    #(#xFC #x11 #xC3 #x01)))


;;; Test Load Instructions

(test i32.load-no-offset-alignment
  "Test serialization of I32.LOAD without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.load stream)

    #(#x28 #x02 #x00)))

(test i32.load-with-offset
  "Test serialization of I32.LOAD with offset"

  (test-encoding stream
    (serialize-instruction '(i32.load (offset 16)) stream)

    #(#x28 #x02 #x10)))

(test i32.load-alignment
  "Test serialization of I32.LOAD with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load (align 1)) stream)

    #(#x28 #x01 #x00)))

(test i32.load-offset-alignment
  "Test serialization of I32.LOAD with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load (offset 5) (align 1)) stream)

    #(#x28 #x01 #x05)))

(test i32.load-errors
  "Test errors in serialization of I32.LOAD"

  (test-error stream
    (serialize-instruction '(i32.load (offset -1)) stream)
    type-error))

(test i64.load-no-offset-alignment
  "Test serialization of I64.LOAD without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load stream)

    #(#x29 #x02 #x00)))

(test i65.load-with-offset
  "Test serialization of I64.LOAD with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load (offset 16)) stream)

    #(#x29 #x02 #x10)))

(test i64.load-alignment
  "Test serialization of I64.LOAD with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load (align 1)) stream)

    #(#x29 #x01 #x00)))

(test i64.load-offset-alignment
  "Test serialization of I64.LOAD with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load (offset 5) (align 1)) stream)

    #(#x29 #x01 #x05)))

(test i64.load-errors
  "Test errors in serialization of I64.LOAD"

  (test-error stream
    (serialize-instruction '(i64.load (offset -1)) stream)
    type-error))

(test f32.load-no-offset-alignment
  "Test serialization of F32.LOAD without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'f32.load stream)

    #(#x2A #x02 #x00)))

(test f32.load-with-offset
  "Test serialization of F32.LOAD with offset"

  (test-encoding stream
    (serialize-instruction '(f32.load (offset 16)) stream)

    #(#x2A #x02 #x10)))

(test F32.load-alignment
  "Test serialization of F32.LOAD with alignment"

  (test-encoding stream
    (serialize-instruction '(f32.load (align 1)) stream)

    #(#x2A #x01 #x00)))

(test f32.load-offset-alignment
  "Test serialization of F32.LOAD with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(f32.load (offset 5) (align 1)) stream)

    #(#x2A #x01 #x05)))

(test f32.load-errors
  "Test errors in serialization of F32.LOAD"

  (test-error stream
    (serialize-instruction '(f32.load (offset -1)) stream)
    type-error))

(test f64.load-no-offset-alignment
  "Test serialization of F64.LOAD without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'f64.load stream)

    #(#x2B #x02 #x00)))

(test f64.load-with-offset
  "Test serialization of F64.LOAD with offset"

  (test-encoding stream
    (serialize-instruction '(f64.load (offset 16)) stream)

    #(#x2B #x02 #x10)))

(test F64.load-alignment
  "Test serialization of F64.LOAD with alignment"

  (test-encoding stream
    (serialize-instruction '(f64.load (align 1)) stream)

    #(#x2B #x01 #x00)))

(test f64.load-offset-alignment
  "Test serialization of F64.LOAD with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(f64.load (offset 5) (align 1)) stream)

    #(#x2B #x01 #x05)))

(test f64.load-errors
  "Test errors in serialization of F64.LOAD"

  (test-error stream
    (serialize-instruction '(f64.load (offset -1)) stream)
    type-error))

(test i32.load8_s-no-offset-alignment
  "Test serialization of I32.LOAD8_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.load8_s stream)

    #(#x2C #x02 #x00)))

(test i32.load8_s-with-offset
  "Test serialization of I32.LOAD8_S with offset"

  (test-encoding stream
    (serialize-instruction '(i32.load8_s (offset 16)) stream)

    #(#x2C #x02 #x10)))

(test i32.load8_s-alignment
  "Test serialization of I32.LOAD8_S with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load8_s (align 1)) stream)

    #(#x2C #x01 #x00)))

(test i32.load8_s-offset-alignment
  "Test serialization of I32.LOAD8_S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load8_s (offset 5) (align 1)) stream)

    #(#x2C #x01 #x05)))

(test i32.load8_s-errors
  "Test errors in serialization of I32.LOAD8_S"

  (test-error stream
    (serialize-instruction '(i32.load8_s (offset -1)) stream)
    type-error))

(test i32.load8_u-no-offset-alignment
  "Test serialization of I32.LOAD8_U without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.load8_u stream)

    #(#x2D #x02 #x00)))

(test i32.load8_u-with-offset
  "Test serialization of I32.LOAD8_U with offset"

  (test-encoding stream
    (serialize-instruction '(i32.load8_u (offset 16)) stream)

    #(#x2D #x02 #x10)))

(test i32.load8_u-alignment
  "Test serialization of I32.LOAD8_U with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load8_u (align 1)) stream)

    #(#x2D #x01 #x00)))

(test i32.load8_u-offset-alignment
  "Test serialization of I32.LOAD8_U with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load8_u (offset 5) (align 1)) stream)

    #(#x2D #x01 #x05)))

(test i32.load8_u-errors
  "Test errors in serialization of I32.LOAD8_U"

  (test-error stream
    (serialize-instruction '(i32.load8_u (offset -1)) stream)
    type-error))

(test i32.load16_s-no-offset-alignment
  "Test serialization of I32.LOAD16_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.load16_s stream)

    #(#x2E #x02 #x00)))

(test i32.load16_s-with-offset
  "Test serialization of I32.LOAD16_S with offset"

  (test-encoding stream
    (serialize-instruction '(i32.load16_s (offset 16)) stream)

    #(#x2E #x02 #x10)))

(test i32.load16_s-alignment
  "Test serialization of I32.LOAD16_S with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load16_s (align 1)) stream)

    #(#x2E #x01 #x00)))

(test i32.load16_s-offset-alignment
  "Test serialization of I32.LOAD16_S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load16_s (offset 5) (align 1)) stream)

    #(#x2E #x01 #x05)))

(test i32.load16_s-errors
  "Test errors in serialization of I32.LOAD16_S"

  (test-error stream
    (serialize-instruction '(i32.load16_s (offset -1)) stream)
    type-error))

(test i32.load16_u-no-offset-alignment
  "Test serialization of I32.LOAD16_U without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.load16_u stream)

    #(#x2F #x02 #x00)))

(test i32.load16_u-with-offset
  "Test serialization of I32.LOAD16_U with offset"

  (test-encoding stream
    (serialize-instruction '(i32.load16_u (offset 16)) stream)

    #(#x2F #x02 #x10)))

(test i32.load16_u-alignment
  "Test serialization of I32.LOAD16_U with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load16_u (align 1)) stream)

    #(#x2F #x01 #x00)))

(test i32.load16_u-offset-alignment
  "Test serialization of I32.LOAD16_U with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.load16_u (offset 5) (align 1)) stream)

    #(#x2F #x01 #x05)))

(test i32.load16_u-errors
  "Test errors in serialization of I32.LOAD16_U"

  (test-error stream
    (serialize-instruction '(i32.load16_u (offset -1)) stream)
    type-error))

(test i64.load8_s-no-offset-alignment
  "Test serialization of I64.LOAD8_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load8_s stream)

    #(#x30 #x02 #x00)))

(test i64.load8_s-with-offset
  "Test serialization of I64.LOAD8_S with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load8_s (offset 16)) stream)

    #(#x30 #x02 #x10)))

(test i64.load8_s-alignment
  "Test serialization of I64.LOAD8_S with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load8_s (align 1)) stream)

    #(#x30 #x01 #x00)))

(test i64.load8_s-offset-alignment
  "Test serialization of I64.LOAD8_S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load8_s (offset 5) (align 1)) stream)

    #(#x30 #x01 #x05)))

(test i64.load8_s-errors
  "Test errors in serialization of I64.LOAD8_S"

  (test-error stream
    (serialize-instruction '(i64.load8_s (offset -1)) stream)
    type-error))

(test i64.load8_u-no-offset-alignment
  "Test serialization of I64.LOAD8_U without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load8_u stream)

    #(#x31 #x02 #x00)))

(test i64.load8_u-with-offset
  "Test serialization of I64.LOAD8_U with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load8_u (offset 16)) stream)

    #(#x31 #x02 #x10)))

(test i64.load8_u-alignment
  "Test serialization of I64.LOAD8_U with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load8_u (align 1)) stream)

    #(#x31 #x01 #x00)))

(test i64.load8_u-offset-alignment
  "Test serialization of I64.LOAD8_U with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load8_u (offset 5) (align 1)) stream)

    #(#x31 #x01 #x05)))

(test i64.load8_u-errors
  "Test errors in serialization of I64.LOAD8_U"

  (test-error stream
    (serialize-instruction '(i64.load8_u (offset -1)) stream)
    type-error))

(test i64.load16_s-no-offset-alignment
  "Test serialization of I64.LOAD16_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load16_s stream)

    #(#x32 #x02 #x00)))

(test i64.load16_s-with-offset
  "Test serialization of I64.LOAD16_S with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load16_s (offset 16)) stream)

    #(#x32 #x02 #x10)))

(test i64.load16_s-alignment
  "Test serialization of I64.LOAD16_S with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load16_s (align 1)) stream)

    #(#x32 #x01 #x00)))

(test i64.load16_s-offset-alignment
  "Test serialization of I64.LOAD16_S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load16_s (offset 5) (align 1)) stream)

    #(#x32 #x01 #x05)))

(test i64.load16_s-errors
  "Test errors in serialization of I64.LOAD16_S"

  (test-error stream
    (serialize-instruction '(i64.load16_s (offset -1)) stream)
    type-error))

(test i64.load16_u-no-offset-alignment
  "Test serialization of I64.LOAD16_U without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load16_u stream)

    #(#x33 #x02 #x00)))

(test i64.load16_u-with-offset
  "Test serialization of I64.LOAD16_U with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load16_u (offset 16)) stream)

    #(#x33 #x02 #x10)))

(test i64.load16_u-alignment
  "Test serialization of I64.LOAD16_U with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load16_u (align 1)) stream)

    #(#x33 #x01 #x00)))

(test i64.load16_u-offset-alignment
  "Test serialization of I64.LOAD16_U with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load16_u (offset 5) (align 1)) stream)

    #(#x33 #x01 #x05)))

(test i64.load16_u-errors
  "Test errors in serialization of I64.LOAD16_U"

  (test-error stream
    (serialize-instruction '(i64.load16_u (offset -1)) stream)
    type-error))

(test i64.load32_s-no-offset-alignment
  "Test serialization of I64.LOAD32_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load32_s stream)

    #(#x34 #x02 #x00)))

(test i64.load32_s-with-offset
  "Test serialization of I64.LOAD32_S with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load32_s (offset 16)) stream)

    #(#x34 #x02 #x10)))

(test i64.load32_s-alignment
  "Test serialization of I64.LOAD32_S with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load32_s (align 1)) stream)

    #(#x34 #x01 #x00)))

(test i64.load32_s-offset-alignment
  "Test serialization of I64.LOAD32_S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load32_s (offset 5) (align 1)) stream)

    #(#x34 #x01 #x05)))

(test i64.load32_s-errors
  "Test errors in serialization of I64.LOAD32_S"

  (test-error stream
    (serialize-instruction '(i64.load32_s (offset -1)) stream)
    type-error))

(test i64.load64_s-no-offset-alignment
  "Test serialization of I64.LOAD64_S without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.load32_u stream)

    #(#x35 #x02 #x00)))

(test i64.load_64s-with-offset
  "Test serialization of I64.LOAD_64S with offset"

  (test-encoding stream
    (serialize-instruction '(i64.load32_u (offset 16)) stream)

    #(#x35 #x02 #x10)))

(test i64.load_64s-alignment
  "Test serialization of I64.LOAD_64S with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load32_u (align 1)) stream)

    #(#x35 #x01 #x00)))

(test i64.load_64s-offset-alignment
  "Test serialization of I64.LOAD_64S with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.load32_u (offset 5) (align 1)) stream)

    #(#x35 #x01 #x05)))

(test i64.load_64s-errors
  "Test errors in serialization of I64.LOAD_64S"

  (test-error stream
    (serialize-instruction '(i64.load32_u (offset -1)) stream)
    type-error))


;;; Test Store Instructions

(test i32.store-no-offset-alignment
  "Test serialization of I32.store without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.store stream)

    #(#x36 #x02 #x00)))

(test i32.store-with-offset
  "Test serialization of I32.store with offset"

  (test-encoding stream
    (serialize-instruction '(i32.store (offset 16)) stream)

    #(#x36 #x02 #x10)))

(test i32.store-alignment
  "Test serialization of I32.store with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store (align 1)) stream)

    #(#x36 #x01 #x00)))

(test i32.store-offset-alignment
  "Test serialization of I32.store with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store (offset 5) (align 1)) stream)

    #(#x36 #x01 #x05)))

(test i32.store-errors
  "Test errors in serialization of I32.store"

  (test-error stream
    (serialize-instruction '(i32.store (offset -1)) stream)
    type-error))

(test i64.store-no-offset-alignment
  "Test serialization of I64.store without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.store stream)

    #(#x37 #x02 #x00)))

(test i64.store-with-offset
  "Test serialization of I64.store with offset"

  (test-encoding stream
    (serialize-instruction '(i64.store (offset 16)) stream)

    #(#x37 #x02 #x10)))

(test i64.store-alignment
  "Test serialization of I64.store with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store (align 1)) stream)

    #(#x37 #x01 #x00)))

(test i64.store-offset-alignment
  "Test serialization of I64.store with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store (offset 5) (align 1)) stream)

    #(#x37 #x01 #x05)))

(test i64.store-errors
  "Test errors in serialization of I64.store"

  (test-error stream
    (serialize-instruction '(i64.store (offset -1)) stream)
    type-error))

(test f32.store-no-offset-alignment
  "Test serialization of F32.store without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'f32.store stream)

    #(#x38 #x02 #x00)))

(test f32.store-with-offset
  "Test serialization of F32.store with offset"

  (test-encoding stream
    (serialize-instruction '(f32.store (offset 16)) stream)

    #(#x38 #x02 #x10)))

(test f32.store-alignment
  "Test serialization of F32.store with alignment"

  (test-encoding stream
    (serialize-instruction '(f32.store (align 1)) stream)

    #(#x38 #x01 #x00)))

(test f32.store-offset-alignment
  "Test serialization of F32.store with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(f32.store (offset 5) (align 1)) stream)

    #(#x38 #x01 #x05)))

(test f32.store-errors
  "Test errors in serialization of F32.store"

  (test-error stream
    (serialize-instruction '(f32.store (offset -1)) stream)
    type-error))

(test f64.store-no-offset-alignment
  "Test serialization of F64.store without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'f64.store stream)

    #(#x39 #x02 #x00)))

(test f64.store-with-offset
  "Test serialization of F64.store with offset"

  (test-encoding stream
    (serialize-instruction '(f64.store (offset 16)) stream)

    #(#x39 #x02 #x10)))

(test F64.store-alignment
  "Test serialization of F64.store with alignment"

  (test-encoding stream
    (serialize-instruction '(f64.store (align 1)) stream)

    #(#x39 #x01 #x00)))

(test f64.store-offset-alignment
  "Test serialization of F64.store with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(f64.store (offset 5) (align 1)) stream)

    #(#x39 #x01 #x05)))

(test f64.store-errors
  "Test errors in serialization of F64.store"

  (test-error stream
    (serialize-instruction '(f64.store (offset -1)) stream)
    type-error))

(test i32.store8-no-offset-alignment
  "Test serialization of I32.store8 without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.store8 stream)

    #(#x3A #x02 #x00)))

(test i32.store8-with-offset
  "Test serialization of I32.store8 with offset"

  (test-encoding stream
    (serialize-instruction '(i32.store8 (offset 16)) stream)

    #(#x3A #x02 #x10)))

(test i32.store8-alignment
  "Test serialization of I32.store8 with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store8 (align 1)) stream)

    #(#x3A #x01 #x00)))

(test i32.store8-offset-alignment
  "Test serialization of I32.store8 with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store8 (offset 5) (align 1)) stream)

    #(#x3A #x01 #x05)))

(test i32.store8-errors
  "Test errors in serialization of I32.store8"

  (test-error stream
    (serialize-instruction '(i32.store8 (offset -1)) stream)
    type-error))

(test i32.store16-no-offset-alignment
  "Test serialization of I32.store16 without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i32.store16 stream)

    #(#x3B #x02 #x00)))

(test i32.store16-with-offset
  "Test serialization of I32.store16 with offset"

  (test-encoding stream
    (serialize-instruction '(i32.store16 (offset 16)) stream)

    #(#x3B #x02 #x10)))

(test i32.store16-alignment
  "Test serialization of I32.store16 with alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store16 (align 1)) stream)

    #(#x3B #x01 #x00)))

(test i32.store16-offset-alignment
  "Test serialization of I32.store16 with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i32.store16 (offset 5) (align 1)) stream)

    #(#x3B #x01 #x05)))

(test i32.store16-errors
  "Test errors in serialization of I32.LOAD16_S"

  (test-error stream
    (serialize-instruction '(i32.store16 (offset -1)) stream)
    type-error))

(test i64.store8-no-offset-alignment
  "Test serialization of I64.store8 without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.store8 stream)

    #(#x3C #x02 #x00)))

(test i64.store8-with-offset
  "Test serialization of I64.store8 with offset"

  (test-encoding stream
    (serialize-instruction '(i64.store8 (offset 16)) stream)

    #(#x3C #x02 #x10)))

(test i64.store8-alignment
  "Test serialization of I64.store8 with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store8 (align 1)) stream)

    #(#x3C #x01 #x00)))

(test i64.store8-offset-alignment
  "Test serialization of I64.store8 with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store8 (offset 5) (align 1)) stream)

    #(#x3C #x01 #x05)))

(test i64.store8-errors
  "Test errors in serialization of I64.store8"

  (test-error stream
    (serialize-instruction '(i64.store8 (offset -1)) stream)
    type-error))

(test i64.store16-no-offset-alignment
  "Test serialization of I64.store16 without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.store16 stream)

    #(#x3D #x02 #x00)))

(test i64.store16-with-offset
  "Test serialization of I64.store16 with offset"

  (test-encoding stream
    (serialize-instruction '(i64.store16 (offset 16)) stream)

    #(#x3D #x02 #x10)))

(test i64.store16-alignment
  "Test serialization of I64.store16 with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store16 (align 1)) stream)

    #(#x3D #x01 #x00)))

(test i64.store16-offset-alignment
  "Test serialization of I64.store16 with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store16 (offset 5) (align 1)) stream)

    #(#x3D #x01 #x05)))

(test i64.store16-errors
  "Test errors in serialization of I64.store16"

  (test-error stream
    (serialize-instruction '(i64.store16 (offset -1)) stream)
    type-error))

(test i64.store32-no-offset-alignment
  "Test serialization of I64.store32 without offset or alignment"

  (test-encoding stream
    (serialize-instruction 'i64.store32 stream)

    #(#x3E #x02 #x00)))

(test i64.store32-with-offset
  "Test serialization of I64.store32 with offset"

  (test-encoding stream
    (serialize-instruction '(i64.store32 (offset 16)) stream)

    #(#x3E #x02 #x10)))

(test i64.store32-alignment
  "Test serialization of I64.store32 with alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store32 (align 1)) stream)

    #(#x3E #x01 #x00)))

(test i64.store32-offset-alignment
  "Test serialization of I64.store32 with offset and alignment"

  (test-encoding stream
    (serialize-instruction '(i64.store32 (offset 5) (align 1)) stream)

    #(#x3E #x01 #x05)))

(test i64.store32-errors
  "Test errors in serialization of I64.store32"

  (test-error stream
    (serialize-instruction '(i64.store32 (offset -1)) stream)
    type-error))


;;; Test Memory Instructions

(test memory.size
  "Test serialization of MEMORY.SIZE"

  (test-encoding stream
    (serialize-instruction 'memory.size stream)
    #(#x3F #x00)))

(test memory.grow
  "Test serialization of MEMORY.GROW"

  (test-encoding stream
    (serialize-instruction 'memory.grow stream)
    #(#x40 #x00)))

(test memory.init
  "Test serialization of MEMORY.INIT"

  (test-encoding stream
    (serialize-instruction '(memory.init #x03) stream)
    #(#xFC #x08 #x03 #x00))

  (test-encoding stream
    (serialize-instruction '(memory.init #xC7) stream)
    #(#xFC #x08 #xC7 #x01 #x00)))

(test data.drop
  "Test serialization of DATA.DROP"

  (test-encoding stream
    (serialize-instruction '(data.drop #x03) stream)
    #(#xFC #x09 #x03))

  (test-encoding stream
    (serialize-instruction '(data.drop #xC7) stream)
    #(#xFC #x09 #xC7 #x01)))

(test memory.copy
  "Test serialization of MEMORY.COPY"

  (test-encoding stream
    (serialize-instruction 'memory.copy stream)
    #(#xFC #x0A #x00 #x00)))

(test memory.fill
  "Test serialization of MEMORY.FILL"

  (test-encoding stream
    (serialize-instruction 'memory.fill stream)
    #(#xFC #x0B #x00)))


;;; Test Constant Instructions

(test i32.const
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

(test i64.const
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

(test f32.const
  "Test serialization of F32.CONST"

  (test-encoding stream
    (serialize-instruction '(f32.const 1.5) stream)

    #(#x43 #x00 #x00 #xC0 #x3F)))

(test f64.const
  "Test serialization of F64.CONST"

  (test-encoding stream
    (serialize-instruction '(f64.const 1.5) stream)

    #(#x44 #x00 #x00 #x00 #x00 #x00 #x00 #xF8 #x3F)))


;;; Test Arithmetic Instructions

(test arithmetic-instructions
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
  (test-encode-instruction f64.reinterpret_i64 #xBF)

  (test-encode-instruction i32.extend8_s #xC0)
  (test-encode-instruction i32.extend16_s #xC1)
  (test-encode-instruction i64.extend8_s #xC2)
  (test-encode-instruction i64.extend16_s #xC3)
  (test-encode-instruction i64.extend32_s #xC4)

  (test-encode-instruction i32.trunc_sat_f32_s #xFC #x00)
  (test-encode-instruction i32.trunc_sat_f32_u #xFC #x01)
  (test-encode-instruction i32.trunc_sat_f64_s #xFC #x02)
  (test-encode-instruction i32.trunc_sat_f64_u #xFC #x03)
  (test-encode-instruction i64.trunc_sat_f32_s #xFC #x04)
  (test-encode-instruction i64.trunc_sat_f32_u #xFC #x05)
  (test-encode-instruction i64.trunc_sat_f64_s #xFC #x06)
  (test-encode-instruction i64.trunc_sat_f64_u #xFC #x07))


;;; Test Serialization of Expressions

(test expressions
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
