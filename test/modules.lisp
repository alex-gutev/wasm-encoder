;;; modules.lisp
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

(defpackage wasm-encoder/test.modules
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

(in-package #:wasm-encoder/test.modules)


;;; Test Suite Definition

(def-suite modules
    :description "Test encoding of modules and section"
    :in wasm-encoder)

(in-suite modules)


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
			     :desc (make-wasm-table :min 2))

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

    #(#x03 #x04				; Section ID 3, 4 bytes
      #x03 #x05 #x01 #x03)))      	; 3 Type Indices: [5, 1, 3]

(test section-table
  "Test serialization of table section"

  (test-encoding stream
    (serialize-table-types
     (list (make-wasm-table :min 2 :max 100))
     stream)

    #(#x04 #x05			  ; Section ID 4, 5 bytes
      #x01 #x70 #x01 2 100)))     ; 1 Table: funcref, min: 2, max: 100

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
     (list
      ;; Active Elements
      ;; Table 0
      ;; Index initialization

      (make-wasm-element
       :index 0
       :offset '((i32.const 2))
       :init
       (make-wasm-element-init-index
	:functions '(5 1 3)))

      (make-wasm-element
       :index 0
       :offset '((i32.const 100))
       :mode :active
       :init
       (make-wasm-element-init-index
      	:functions '(9 8)))

      ;; Active element
      ;; Table 5
      ;; Index initialization

      (make-wasm-element
       :index 5
       :offset '((i32.const 7))
       :init
       (make-wasm-element-init-index
      	:functions '(#xFC 1 3)))

      ;; Active element
      ;; Table 0
      ;; Expression initialization

      (make-wasm-element
       :index 0
       :offset '((i32.const #xB6))
       :mode :active
       :init
       (make-wasm-element-init-expressions
      	:type 'funcref
      	:expressions '(((ref.func 0)) ((ref.func 9)))))


      ;; Active element
      ;; Table 2
      ;; Expression initialization

      (make-wasm-element
       :index 2
       :offset '((i32.const #xB6))
       :mode :active
       :init
       (make-wasm-element-init-expressions
      	:type 'funcref
      	:expressions '(((ref.func 0)) ((ref.func 6))))))

     stream)

    #(#x09 #x36				; Section ID 9
      #x05				; Five Element Segments

      ;; Element 1

      #x00				; Table 0
      #x41 #x02 #x0B			; i32.const 2
      #x03 5 1 3			; Functions [5, 1, 3]

      ;; Element 2

      #x00				; Table 0
      #x41 #xE4 #x0 #x0B		; i32.const 100
      #x02 9 8				; Functions [9, 8]

      ;; Element 4

      #x02 #x05				; Active element table 5
      #x41 #x07 #x0B			; i32.const 7
      #x00
      #x03 #xFC #x01 #x01 #x03		; Functions [0xFC, 1, 3]

      ;; Element 6

      #x04				 ; Active Element table 0
      #x41 #xB6 #x01 #x0B		 ; i32.const #xB6
      #x02 #xD2 #x00 #x0B #xD2 #x09 #x0B ; FUNC.REF Expressions [0, 9]


      ;; Element 8

      #x06 #x02				    ; Active Element table 2
      #x41 #xB6 #x01 #x0B		    ; i32.const #xB6
      #x70				    ; Type FUNCREF
      #x02 #xD2 #x00 #x0B #xD2 #x06 #x0B))) ; FUNC.REF Expressions [0, 6]

(test section-element-passive
  "Test serialization of element sections with passive elements"

  (test-encoding stream
    (serialize-table-elements
     (list
      ;; Passive Element
      ;; Index initialization

      (make-wasm-element
       :mode :passive
       :init
       (make-wasm-element-init-index
      	:functions '(1 2 3)))

      ;; Passive element
      ;; Expression initialization

      (make-wasm-element
       :offset '((i32.const #xB6))
       :mode :passive
       :init
       (make-wasm-element-init-expressions
      	:type 'externref
      	:expressions '(((ref.func 0)) ((ref.func 9))))))
     stream)

    #(#x09 #x10				; Section ID 9
      #x02				; Two Table Element Segments

      ;; Element 1

      #x01 #x00				; Passive Element
      #x03 #x01 #x02 #x03		; Functions [1, 2, 3]

      ;; Element 2

      #x05				; Passive Element
      #x6F				; Type EXTERNREF
      #x02 #xD2 #x00 #x0B #xD2 #x09 #x0B)))

(test section-element-declarative
  "Test serialization of element sections with declarative elements"

  (test-encoding stream
    (serialize-table-elements
     (list
      ;; Declarative element
      ;; Index initialization

      (make-wasm-element
       :mode :declarative
       :init
       (make-wasm-element-init-index
      	:functions '(1 2 3)))

      ;; Declarative element
      ;; Expression initialization

      (make-wasm-element
       :offset '((i32.const #xB6))
       :mode :declarative
       :init
       (make-wasm-element-init-expressions
      	:type 'funcref
      	:expressions '(((ref.func 0)) ((ref.func 7))))))

     stream)

    #(#x09 #x10				; Section ID 9
      #x02				; Two Table Element Segments

      ;; Element 1

      #x03 #x00				; Declarative Element
      #x03 #x01 #x02 #x03		; Functions [1, 2, 3]

      ;; Element 2

      #x07				    ; Declarative Element
      #x70				    ; Type FUNCREF
      #x02 #xD2 #x00 #x0B #xD2 #x07 #x0B))) ; FUNC.REF Expressions [0, 9]

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
			   :memory 0)

	   (make-wasm-data :offset '((i32.const 3))
			   :bytes #(1 2 3 4 5)
			   :memory #x8C)

	   (make-wasm-data :mode :passive
			   :bytes #(1 2 3 4 5)))

     stream)

    #(#x0B #x25				; Section ID 11
      #x04				; 2 Data Segments

      #x00
      #x41 #x10 #x0B			; i32.const 16
      #x04 1 5 9 45

      #x00
      #x41 #x0 #x0B			; i32.const 0
      #x03 7 8 9

      #x02
      #x8C #x01
      #x41 #x03 #x0B
      #x05 1 2 3 4 5

      #x01
      #x05 1 2 3 4 5)))
