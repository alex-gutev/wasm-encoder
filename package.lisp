;;; package.lisp
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


(defpackage #:wasm-encoder
  (:use #:generic-cl
	#:agutil
	#:alexandria
	#:trivia

	#:ieee-floats
	#:babel
	#:babel-encodings)

  (:shadowing-import-from #:generic-cl
                          #:emptyp
                          #:multiply
                          #:accumulate)

  (:import-from #:flexi-streams
                #:with-output-to-sequence)

  (:export
   ;; Types

   #:wasm-module
   #:wasm-module-types
   #:wasm-module-imports
   #:wasm-module-functions
   #:wasm-module-tabls
   #:wasm-module-memory
   #:wasm-module-globals
   #:wasm-module-exports
   #:wasm-module-start
   #:wasm-module-elements
   #:wasm-module-data
   #:wasm-module-p
   #:make-wasm-module

   #:wasm-function-type
   #:wasm-function-type-params
   #:wasm-function-type-results
   #:wasm-function-type-p
   #:make-wasm-function-type

   #:wasm-import
   #:wasm-import-module
   #:wasm-import-name
   #:wasm-import-type
   #:wasm-import-desc
   #:wasm-import-p
   #:make-wasm-import

   #:wasm-export
   #:wasm-export-name
   #:wasm-export-type
   #:wasm-export-index
   #:wasm-export-p
   #:make-wasm-export

   #:wasm-limit
   #:wasm-limit-min
   #:wasm-limit-max
   #:wasm-limit-p
   #:make-wasm-limit

   #:wasm-global
   #:wasm-global-type
   #:wasm-global-mutable-p
   #:wasm-global-init
   #:wasm-global-p
   #:make-wasm-global

   #:wasm-table
   #:wasm-table-index
   #:wasm-table-offset
   #:wasm-table-functions
   #:wasm-table-p
   #:make-wasm-table

   #:wasm-function
   #:wasm-function-type
   #:wasm-function-locals
   #:wasm-function-code
   #:wasm-function-p
   #:make-wasm-function

   #:wasm-data
   #:wasm-data-offset
   #:wasm-data-bytes
   #:wasm-data-memory
   #:wasm-data-p
   #:make-wasm-data

   ;; Functions

   #:serialize-module))
