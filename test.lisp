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

(defpackage wasm-encoder.test
  (:use #:generic-cl
	#:wasm-encoder
	#:agutil
	#:alexandria
	#:trivia

	#:prove)

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

(in-package #:wasm-encoder.test)

(defmacro test-encoding (stream &body (form expected))
  `(is (with-output-to-sequence (,stream) ,form)
       ,expected
       :test #'equalp))

(defmacro test-error (stream &body (form type))
  `(is-error (with-output-to-sequence (,stream) ,form) ,type
	     ,(format nil "~a results in ~a" form type)))

(defmacro test-encode-instruction (op opcode)
  `(subtest ,(string-downcase (symbol-name op))
     (test-encoding stream
       (serialize-instruction ',op stream)
       #(,opcode))))

(plan nil)

(subtest "Internal Unit Tests"
  (subtest "Values"
    (subtest "Integers"
      (subtest "Unsigned"
	(test-encoding stream (serialize-u32 #x18 stream) #(#x18))
	(test-encoding stream (serialize-u32 #x7D stream) #(#x7D))
	(test-encoding stream (serialize-u32 #xA52 stream) #(#xD2 #x14))

	(test-encoding stream
	  (serialize-u32 #xA5B23408 stream)
	  #(#x88 #xE8 #xC8 #xAD #x0A))

	(test-encoding stream
	  (serialize-u32 (1- (expt 2 32)) stream)
	  #(#xFF #xFF #xFF #xFF #x0F))

	(subtest "Range Checks"
	  (test-error stream (serialize-u32 (expt 2 32) stream) type-error)
	  (test-error stream (serialize-u32 -10 stream) type-error)))

      (subtest "Signed"
	(test-encoding stream (serialize-i32 #x18 stream) #(#x18))
	(test-encoding stream (serialize-i32 #x7D stream) #(#x7D))
	(test-encoding stream (serialize-i32 #xA52 stream) #(#xD2 #x14))

	(test-encoding stream (serialize-i32 -15 stream) #(#x71))
	(test-encoding stream (serialize-i32 -1234 stream) #(#xAE #x76))

	(subtest "Range Checks"
	  (test-error stream (serialize-i32 (expt 2 31) stream) type-error)
	  (test-error stream (serialize-i32 (- (1+ (expt 2 31))) stream) type-error))))

    (subtest "Floating Point"
      (subtest "Single - 32 Bit"
	(test-encoding stream (serialize-float 7.125 stream) #(#x00 #x00 #xE4 #x40))
	(test-encoding stream (serialize-float 12.576 stream) #(#x4c #x37 #x49 #x41))

	(test-encoding stream (serialize-float -7.125 stream) #(#x00 #x00 #xE4 #xC0))
	(test-encoding stream (serialize-float -12.576 stream) #(#x4c #x37 #x49 #xC1))

	(test-error stream (serialize-float 112.765d0 stream) type-error))

      (subtest "Double - 64 Bit"
	(test-encoding stream (serialize-double-float 35.0625d0 stream)
		       #(#x00 #x00 #x00 #x00 #x00 #x88 #x41 #x40))

	(test-encoding stream (serialize-double-float 163.8973d0 stream)
		       #(#x6D #x56 #x7D #xAE #xB6 #x7C #x64 #x40))


	(test-encoding stream (serialize-double-float -35.0625d0 stream)
		       #(#x00 #x00 #x00 #x00 #x00 #x88 #x41 #xC0))

	(test-encoding stream (serialize-double-float -163.8973d0 stream)
		       #(#x6D #x56 #x7D #xAE #xB6 #x7C #x64 #xC0))))

    (subtest "Strings"
      (test-encoding stream
	(serialize-string "hello" stream)
	#(#x05 #x68 #x65 #x6C #x6C #x6F))

      ;; Test non-ASCII characters
      (test-encoding stream
	(serialize-string "Уникод" stream)
	#(#x0C #xD0 #xA3 #xD0 #xBD #xD0 #xB8 #xD0 #xBA #xD0 #xBE #xD0 #xB4)))

    (subtest "Vectors"
      (test-encoding stream
	(serialize-vector #'write-byte #(#x0A #xB0 #xCD) stream)
	#(#x03 #x0A #xB0 #xCD))

      (test-encoding stream
	(serialize-vector #'write-byte #(#x0A) stream)
	#(#x01 #x0A))

      (test-encoding stream
	(serialize-vector #'write-byte #() stream)
	#(#x00))))

  (subtest "Types"
    (subtest "Value Types"
      (test-encoding stream (serialize-type 'i32 stream) #(#x7F))
      (test-encoding stream (serialize-type 'i64 stream) #(#x7E))
      (test-encoding stream (serialize-type 'f32 stream) #(#x7D))
      (test-encoding stream (serialize-type 'f64 stream) #(#x7C))

      (subtest "Errors"
	(test-error stream (serialize-type 'not-a-type stream) error)))

    (subtest "Function Types"
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

	#(#x60 #x00 #x00)))

    (subtest "Limits"
      (test-encoding stream

	(serialize-limit
	 (make-wasm-limit :min 3)
	 stream)

	#(#x00 #x03))

      (test-encoding stream

	(serialize-limit
	 (make-wasm-limit :min 100 :max 350)
	 stream)

	#(#x01 100 #xDE #x02))

      (subtest "Errors"
	(test-error stream

	  (serialize-limit
	   (make-wasm-limit :min -3 :max (expt 2 35))
	   stream)

	  type-error)))

    (subtest "Memory Types"
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

    (subtest "Table Types"
      (test-encoding stream
	(serialize-table
	 (make-wasm-limit :min 10)
	 stream)

	#(#x70 #x00 10))

      (test-encoding stream
	(serialize-table
	 (make-wasm-limit :min 10 :max 100)
	 stream)

	#(#x70 #x01 10 100)))

    (subtest "Global Types"
      (test-encoding stream
	(serialize-global 'i32 nil stream)

	#(#x7F #x00))

      (test-encoding stream
	(serialize-global 'i64 t stream)

	#(#x7E #x01))))

  (subtest "Instructions"
    (subtest "Control Instructions"
      (subtest "unreachable"
	(test-encoding stream
	  (serialize-instruction 'unreachable stream)
	  #(#x00)))

      (subtest "nop"
	(test-encoding stream
	  (serialize-instruction 'nop stream)
	  #(#x01)))

      (subtest "return"
	(test-encoding stream
	  (serialize-instruction 'return stream)
	  #(#x0F))))

    (subtest "Branch Instructions"
      (subtest "br"
	(test-encoding stream
	  (serialize-instruction '(br 0) stream)
	  #(#x0C 0))

	(test-encoding stream
	  (serialize-instruction '(br 10) stream)
	  #(#x0C 10))

	(test-encoding stream
	  (serialize-instruction '(br 300) stream)
	  #(#x0C #xAC #x02))

	(subtest "Errors"
	  (test-error stream
	    (serialize-instruction '(br -10) stream)
	    type-error)))

      (subtest "br_if"
	(test-encoding stream
	  (serialize-instruction '(br_if 0) stream)
	  #(#x0D 0))

	(test-encoding stream
	  (serialize-instruction '(br_if 10) stream)
	  #(#x0D 10))

	(test-encoding stream
	  (serialize-instruction '(br_if 300) stream)
	  #(#x0D #xAC #x02))

	(subtest "Errors"
	  (test-error stream
	    (serialize-instruction '(br_if -10) stream)
	    type-error)))

      (subtest "br_table"
	(test-encoding stream
	  (serialize-instruction '(br_table 1 0 5) stream)

	  #(#x0E #x02 1 0 5))

	(test-encoding stream
	  (serialize-instruction '(br_table 1 5 300 0 4) stream)

	  #(#x0E #x04 1 5 #xAC #x02 0 4))

	(subtest "Errors"
	  (test-error stream
	    (serialize-instruction '(br_table 1 6 -3 4) stream)
	    type-error))))

    (subtest "Call Instructions"
      (subtest "call"
	(test-encoding stream
	  (serialize-instruction '(call 0) stream)
	  #(#x10 0))

	(test-encoding stream
	  (serialize-instruction '(call 10) stream)
	  #(#x10 10))

	(test-encoding stream
	  (serialize-instruction '(call 301) stream)
	  #(#x10 #xAD #x02))

	(subtest "Errors"
	  (test-error stream
	    (serialize-instruction '(call -10) stream)
	    type-error)))

      (subtest "call_indirect"
	(test-encoding stream
	  (serialize-instruction '(call_indirect 0) stream)
	  #(#x11 0 #x00))

	(test-encoding stream
	  (serialize-instruction '(call_indirect 10) stream)
	  #(#x11 10 #x00))

	(test-encoding stream
	  (serialize-instruction '(call_indirect 301) stream)
	  #(#x11 #xAD #x02 #x00))

	(subtest "Errors"
	  (test-error stream
	    (serialize-instruction '(call_indirect -10) stream)
	    type-error))))

    (subtest "Structured Blocks"
      (subtest "block"
	(subtest "No Result Type"
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

	(subtest "With Result Type"
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
	      #x0B))))

      (subtest "loop"
	(subtest "No Result Type"
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

	(subtest "With Result Type"
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
	      #x0B))))

      (subtest "if"
	(subtest "Without Else Branch"
	  (subtest "No Result Type"
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

	  (subtest "With Result Type"
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
		#x0B))))

	(subtest "With Else Branch"
	  (subtest "No Result Type"
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

	  (subtest "With Result Type"
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

		#x0B))))))

    (subtest "Parametric Instructions"
      (subtest "drop"
	(test-encoding stream
	  (serialize-instruction 'drop stream)
	  #(#x1A)))

      (subtest "select"
	(test-encoding stream
	  (serialize-instruction 'select stream)
	  #(#x1B))))

    (subtest "Variable Instructions"
      (subtest "local.get"
	(test-encoding stream
	  (serialize-instruction '(local.get 5) stream)
	  #(#x20 #x05))

	(test-encoding stream
	  (serialize-instruction '(local.get 301) stream)
	  #(#x20 #xAD #x02))

	(test-error stream
	  (serialize-instruction '(local.get -5) stream)
	  type-error))

      (subtest "local.set"
	(test-encoding stream
	  (serialize-instruction '(local.set 5) stream)
	  #(#x21 #x05))

	(test-encoding stream
	  (serialize-instruction '(local.set 301) stream)
	  #(#x21 #xAD #x02))

	(test-error stream
	  (serialize-instruction '(local.set -5) stream)
	  type-error))

      (subtest "local.tee"
	(test-encoding stream
	  (serialize-instruction '(local.tee 5) stream)
	  #(#x22 #x05))

	(test-encoding stream
	  (serialize-instruction '(local.tee 301) stream)
	  #(#x22 #xAD #x02))

	(test-error stream
	  (serialize-instruction '(local.tee -5) stream)
	  type-error))

      (subtest "global.get"
	(test-encoding stream
	  (serialize-instruction '(global.get 5) stream)
	  #(#x23 #x05))

	(test-encoding stream
	  (serialize-instruction '(global.get 301) stream)
	  #(#x23 #xAD #x02))

	(test-error stream
	  (serialize-instruction '(global.get -5) stream)
	  type-error))

      (subtest "global.set"
	(test-encoding stream
	  (serialize-instruction '(global.set 5) stream)
	  #(#x24 #x05))

	(test-encoding stream
	  (serialize-instruction '(global.set 301) stream)
	  #(#x24 #xAD #x02))

	(test-error stream
	  (serialize-instruction '(global.set -5) stream)
	  type-error)))

    (subtest "Memory Instructions"
      (subtest "Load Instructions"
	(subtest "i32.load"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i32.load stream)

	      #(#x28 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i32.load (offset 16)) stream)

	      #(#x28 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.load (align 1)) stream)

	      #(#x28 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.load (offset 5) (align 1)) stream)

	      #(#x28 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i32.load (offset -1)) stream)
	      type-error)))

	(subtest "i64.load"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.load stream)

	      #(#x29 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.load (offset 16)) stream)

	      #(#x29 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load (align 1)) stream)

	      #(#x29 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load (offset 5) (align 1)) stream)

	      #(#x29 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.load (offset -1)) stream)
	      type-error)))

	(subtest "f32.load"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'f32.load stream)

	      #(#x2A #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(f32.load (offset 16)) stream)

	      #(#x2A #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(f32.load (align 1)) stream)

	      #(#x2A #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(f32.load (offset 5) (align 1)) stream)

	      #(#x2A #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(f32.load (offset -1)) stream)
	      type-error)))

	(subtest "f64.load"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'f64.load stream)

	      #(#x2B #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(f64.load (offset 16)) stream)

	      #(#x2B #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(f64.load (align 1)) stream)

	      #(#x2B #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(f64.load (offset 5) (align 1)) stream)

	      #(#x2B #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(f64.load (offset -1)) stream)
	      type-error)))

	(subtest "i32.load8_s"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i32.load8_s stream)

	      #(#x2C #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i32.load8_s (offset 16)) stream)

	      #(#x2C #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.load8_s (align 1)) stream)

	      #(#x2C #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.load8_s (offset 5) (align 1)) stream)

	      #(#x2C #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i32.load8_s (offset -1)) stream)
	      type-error)))

	(subtest "i32.load8_u"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i32.load8_u stream)

	      #(#x2D #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i32.load8_u (offset 16)) stream)

	      #(#x2D #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.load8_u (align 1)) stream)

	      #(#x2D #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.load8_u (offset 5) (align 1)) stream)

	      #(#x2D #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i32.load8_u (offset -1)) stream)
	      type-error)))

	(subtest "i32.load16_s"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i32.load16_s stream)

	      #(#x2E #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i32.load16_s (offset 16)) stream)

	      #(#x2E #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.load16_s (align 1)) stream)

	      #(#x2E #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.load16_s (offset 5) (align 1)) stream)

	      #(#x2E #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i32.load16_s (offset -1)) stream)
	      type-error)))

	(subtest "i32.load16_u"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i32.load16_u stream)

	      #(#x2F #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i32.load16_u (offset 16)) stream)

	      #(#x2F #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.load16_u (align 1)) stream)

	      #(#x2F #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.load16_u (offset 5) (align 1)) stream)

	      #(#x2F #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i32.load16_u (offset -1)) stream)
	      type-error)))

	(subtest "i64.load8_s"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.load8_s stream)

	      #(#x30 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.load8_s (offset 16)) stream)

	      #(#x30 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load8_s (align 1)) stream)

	      #(#x30 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load8_s (offset 5) (align 1)) stream)

	      #(#x30 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.load8_s (offset -1)) stream)
	      type-error)))

	(subtest "i64.load8_u"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.load8_u stream)

	      #(#x31 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.load8_u (offset 16)) stream)

	      #(#x31 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load8_u (align 1)) stream)

	      #(#x31 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load8_u (offset 5) (align 1)) stream)

	      #(#x31 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.load8_u (offset -1)) stream)
	      type-error)))

	(subtest "i64.load16_s"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.load16_s stream)

	      #(#x32 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.load16_s (offset 16)) stream)

	      #(#x32 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load16_s (align 1)) stream)

	      #(#x32 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load16_s (offset 5) (align 1)) stream)

	      #(#x32 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.load16_s (offset -1)) stream)
	      type-error)))

	(subtest "i64.load16_u"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.load16_u stream)

	      #(#x33 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.load16_u (offset 16)) stream)

	      #(#x33 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load16_u (align 1)) stream)

	      #(#x33 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load16_u (offset 5) (align 1)) stream)

	      #(#x33 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.load16_u (offset -1)) stream)
	      type-error)))

	(subtest "i64.load32_s"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.load32_s stream)

	      #(#x34 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.load32_s (offset 16)) stream)

	      #(#x34 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load32_s (align 1)) stream)

	      #(#x34 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load32_s (offset 5) (align 1)) stream)

	      #(#x34 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.load32_s (offset -1)) stream)
	      type-error)))

	(subtest "i64.load32_u"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.load32_u stream)

	      #(#x35 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.load32_u (offset 16)) stream)

	      #(#x35 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load32_u (align 1)) stream)

	      #(#x35 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.load32_u (offset 5) (align 1)) stream)

	      #(#x35 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.load32_u (offset -1)) stream)
	      type-error))))

      (subtest "Store Instructions"
	(subtest "i32.store"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i32.store stream)

	      #(#x36 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i32.store (offset 16)) stream)

	      #(#x36 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.store (align 1)) stream)

	      #(#x36 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.store (offset 5) (align 1)) stream)

	      #(#x36 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i32.store (offset -1)) stream)
	      type-error)))

	(subtest "i64.store"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.store stream)

	      #(#x37 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.store (offset 16)) stream)

	      #(#x37 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.store (align 1)) stream)

	      #(#x37 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.store (offset 5) (align 1)) stream)

	      #(#x37 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.store (offset -1)) stream)
	      type-error)))

	(subtest "f32.store"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'f32.store stream)

	      #(#x38 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(f32.store (offset 16)) stream)

	      #(#x38 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(f32.store (align 1)) stream)

	      #(#x38 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(f32.store (offset 5) (align 1)) stream)

	      #(#x38 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(f32.store (offset -1)) stream)
	      type-error)))

	(subtest "f64.store"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'f64.store stream)

	      #(#x39 #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(f64.store (offset 16)) stream)

	      #(#x39 #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(f64.store (align 1)) stream)

	      #(#x39 #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(f64.store (offset 5) (align 1)) stream)

	      #(#x39 #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(f64.store (offset -1)) stream)
	      type-error)))

	(subtest "i32.store8"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i32.store8 stream)

	      #(#x3A #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i32.store8 (offset 16)) stream)

	      #(#x3A #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.store8 (align 1)) stream)

	      #(#x3A #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.store8 (offset 5) (align 1)) stream)

	      #(#x3A #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i32.store8 (offset -1)) stream)
	      type-error)))

	(subtest "i32.store16"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i32.store16 stream)

	      #(#x3B #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i32.store16 (offset 16)) stream)

	      #(#x3B #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.store16 (align 1)) stream)

	      #(#x3B #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i32.store16 (offset 5) (align 1)) stream)

	      #(#x3B #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i32.store16 (offset -1)) stream)
	      type-error)))

	(subtest "i64.store8"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.store8 stream)

	      #(#x3C #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.store8 (offset 16)) stream)

	      #(#x3C #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.store8 (align 1)) stream)

	      #(#x3C #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.store8 (offset 5) (align 1)) stream)

	      #(#x3C #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.store8 (offset -1)) stream)
	      type-error)))

	(subtest "i64.store16"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.store16 stream)

	      #(#x3D #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.store16 (offset 16)) stream)

	      #(#x3D #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.store16 (align 1)) stream)

	      #(#x3D #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.store16 (offset 5) (align 1)) stream)

	      #(#x3D #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.store16 (offset -1)) stream)
	      type-error)))

	(subtest "i64.store32"
	  (subtest "No Offset or Alignment"
	    (test-encoding stream
	      (serialize-instruction 'i64.store32 stream)

	      #(#x3E #x02 #x00)))

	  (subtest "With Offset"
	    (test-encoding stream
	      (serialize-instruction '(i64.store32 (offset 16)) stream)

	      #(#x3E #x02 #x10)))

	  (subtest "With Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.store32 (align 1)) stream)

	      #(#x3E #x01 #x00)))

	  (subtest "With Offset and Alignment"
	    (test-encoding stream
	      (serialize-instruction '(i64.store32 (offset 5) (align 1)) stream)

	      #(#x3E #x01 #x05)))

	  (subtest "Errors"
	    (test-error stream
	      (serialize-instruction '(i64.store32 (offset -1)) stream)
	      type-error))))

      (subtest "Other"
	(subtest "memory.size"
	  (test-encoding stream
	    (serialize-instruction 'memory.size stream)
	    #(#x3F #x00)))

	(subtest "memory.grow"
	  (test-encoding stream
	    (serialize-instruction 'memory.grow stream)
	    #(#x40 #x00)))))

    (subtest "Numeric Constants"
      (subtest "i32.const"
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

	  #(#x41 #xFF #xFF #xFF #xFF #x0F)))

      (subtest "i64.const"
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

	  #(#x42 #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #xFF #x01)))

      (subtest "f32.const"
	(test-encoding stream
	  (serialize-instruction '(f32.const 1.5) stream)

	  #(#x43 #x00 #x00 #xC0 #x3F)))

      (subtest "f64.const"
	(test-encoding stream
	  (serialize-instruction '(f64.const 1.5) stream)

	  #(#x44 #x00 #x00 #x00 #x00 #x00 #x00 #xF8 #x3F))))

    (subtest "Arithmetic Instructions"
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
      (test-encode-instruction f64.reinterpret_i64 #xBF)))

  (subtest "Expressions"
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

  (subtest "Modules"
    (subtest "Type Section"
      (test-encoding stream
	(serialize-types
	 (list (make-wasm-function-type :params '(i32 i32) :results '(i32))
	       (make-wasm-function-type :results '(i32)))

	 stream)

	#(#x01 #x0B #x02
	  #x60 #x02 #x7F #x7F #x01 #x7F
	  #x60 #x00 #x01 #x7F)))

    (subtest "Imports Section"
      (test-encoding stream
	(serialize-imports
	 (list (make-wasm-import :module "runtime"
				 :name "fn"
				 :type :func
				 :desc 5)

	       (make-wasm-import :module "env"
				 :name "table"
				 :type :table
				 :desc (make-wasm-limit :min 2))

	       (make-wasm-import :module "env"
				 :name "memory"
				 :type :memory
				 :desc (make-wasm-limit :min 1))

	       (make-wasm-import :module "env"
				 :name "stack"
				 :type :global
				 :desc '(i32 t)))
	 stream)

	#(#x02 #x37			; Section ID 2, 54 bytes
	  #x04				; 4 Imports

	  ;; Import 1: runtime.fn
	  #x07 #x72 #x75 #x6E #x74 #x69 #x6D #x65 ; "runtime"
	  #x02 #x66 #x6E			  ; "fn"
	  #x00 #x05				  ; Function type 5

	  ;; Import 2: env.table
	  #x03 #x65 #x6E #x76		; "env"
	  #x05 #x74 #x61 #x62 #x6C #x65	; "table"
	  #x01 #x70 #x00 #x02		; Table type funcref, min: 0

	  ;; Import 3: env.memory
	  #x03 #x65 #x6E #x76		     ; "env"
	  #x06 #x6D #x65 #x6D #x6F #x72 #x79 ; "memory"
	  #x02 #x00 #x01		     ; Memory type, min: 0

	  ;; Import 4: env.stack
	  #x03 #x65 #x6E #x76		; "env"
	  #x05 #x73 #x74 #x61 #x63 #x6B	; "stack"
	  #x03 #x7F #x01)))		; Global type i32 mutable

    (subtest "Function Section"
      (test-encoding stream
	(serialize-function-types '(5 1 3) stream)

	#(#x03 #x04			; Section ID 3, 4 bytes
	  #x03 #x05 #x01 #x03)))	; 3 Type Indices: [5, 1, 3]

    (subtest "Table Section"
      (test-encoding stream
	(serialize-table-types
	 (list (make-wasm-limit :min 2 :max 100))
	 stream)

	#(#x04 #x05		  ; Section ID 4, 5 bytes
	  #x01 #x70 #x01 2 100))) ; 1 Table: funcref, min: 2, max: 100

    (subtest "Memory Section"
      (test-encoding stream
	(serialize-memory-types
	 (list (make-wasm-limit :min 3))
	 stream)

	#(#x05 #x03			; Section ID 5, 3 bytes
	  #x01 #x00 3)))		; 1 Memory, min: 3

    (subtest "Global Section"
      (test-encoding stream
	(serialize-global-section
	 (list (make-wasm-global :type 'i32 :init '((i32.const 3)))
	       (make-wasm-global :type 'i64 :mutable-p t))
	 stream)

	#(#x06	#x09			; Section ID 6, 9 bytes
	  #x02				; 2 Globals

	  #x7F #x00 #x41 3 #x0B		; I32 Immutable Global
	  #x7E #x01 #x0B)))		; I64 Mutable Global

    (subtest "Exports Section"
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

    (subtest "Start Section"
      (test-encoding stream
	(serialize-start-section 5 stream)

	#(#x08 #x01 #x05)))

    (subtest "Table Elements"
      (test-encoding stream
	(serialize-table-elements
	 (list (make-wasm-table :index 0
				:offset '((i32.const 2))
				:functions '(5 1 3))
	       (make-wasm-table :index 0
				:offset '((i32.const 100))
				:functions '(9 8)))
	 stream)

	#(#x09 #x10		   ; Section ID 9, 16 Bytes
	  #x02			   ; Two Table Element Initializations

	  #x00				; Table 0
	  #x41 #x02 #x0B		; i32.const 2
	  #x03 5 1 3			; 3 Functions [5, 1, 3]

	  #x00				; Table 0
	  #x41 100 #x0B			; i32.const 100
	  #x02 9 8)))			; 2 Functions [5, 1, 3]

    (subtest "Code Section"
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

    (subtest "Data Section"
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
	  #x03 7 8 9)))))

(finalize)
