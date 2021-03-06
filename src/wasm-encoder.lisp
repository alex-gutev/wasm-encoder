;;; wasm-encoder.lisp
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

(in-package #:wasm-encoder)


;;; Types

(defstruct wasm-module
  "Represents a WebAssembly module that can be serialized to the binary wasm format.

   Each slot contains a list of the entities comprising a particular
   section of the module. The entities are serialized in the order
   given in the list.

   TYPES is a list of `WASM-FUNCTION-TYPE' objects representing the
   function type signatures of the module's functions.

   IMPORTS is a list of `WASM-IMPORT' objects representing the
   module's imports.

   FUNCTIONS is a list of `WASM-FUNCTION' objects representing the
   module's functions, including both the signatures and code.

   TABLES is a list of `WASM-TABLE' objects specifying the limits, and
   types, of the module's tables. In the current version of
   WebAssembly, a module may have at most one table of type FUNCREF,
   however this library supports multiple tables of both type FUNCREF
   and EXTERNREF.

   MEMORY is a list of `WASM-LIMIT' objects specifying the limits of
   the module's memory objects. Currently a module may have at most
   one memory object, however this library supports multiple memory
   objects.

   GLOBALS is a list of `WASM-GLOBAL' objects representing the
   module's global variables.

   EXPORTS is a list `WASM-EXPORT' objects representing the module's
   exports.

   START is the index of the function serving as the module's entry
   point. If NIL the module does not have an entry point.

   ELEMENTS is a list of `WASM-ELEMET' objects representing the
   module's element segments, for initializing the module's tables.

   DATA is a list of `WASM-DATA' objects representing the module's
   data segments for initializing the module's memory.

   DATA-COUNT is a flag, which if true, a data count section is
   emitted, prior to the code section, storing the number of data
   segments."

  types
  imports
  functions
  tables
  memory
  globals
  exports
  start
  elements
  data
  (data-count t))

(defstruct wasm-function-type
  "Represents a function type signature.

   PARAMS is a list of the argument types.

   RESULTS is a list of the return value types."

  params
  results)

(defstruct wasm-import
  "Represents an imported entity.

   The two-level import name is given by MODULE and NAME.

   TYPE is a keyword specifying the kind of entity that is imported:

     :FUNC   - function
     :TABLE  - table object
     :MEMORY - memory object
     :GLOBAL - global variable

   DESC is a description of the imported entity, which depends on TYPE:

     :FUNC - Index of the function's type signature within the
             module's type section.

     :TABLE - A `WASM-TABLE' object specifying the table type and
              limits.

     :MEMORY - A `WASM-LIMIT' object specifying the memory limits.

     :GLOBAL - A list of the form (TYPE MUTABLE-P) where TYPE is the
               value type of the variable and MUTABLE-P is a flag for
               whether the variable is mutable."

  module
  name
  type
  desc)

(defstruct wasm-export
  "Represents an exported entity.

   NAME is the name at which the entity is exported.

   TYPE is a keyword specifying the kind of entity that is exported:

     :FUNC   - function
     :TABLE  - table object
     :MEMORY - memory object
     :GLOBAL - global variable

   INDEX is the index of the exported entity within the module
   section in which it is contained."

  name
  type
  index)


(defstruct wasm-limit
  "Represents a limit of memory or table object.

   MIN is the lower bound, which must be greater than or equal to 0.

   MAX is the upper bound. If NIL the limit has no upper bound."

  min
  max)

(defstruct wasm-global
  "Represents a global variable.

   TYPE is the value type of the variable.

   MUTABLE-P is a flag, which if true indicates the variable is
   mutable.

   INIT is an optional expression (a list of instructions) which
   compute the variables initial value. If NIL the variable has no
   initial value."

  type
  mutable-p
  init)

(defstruct wasm-element
  "Represents an element segment.

   Element segments specify the initial values of table elements.

   MODE is a keyword specifying the element segment mode:

    :ACTIVE      - An active element initialized on instantiation (the default)
    :PASSIVE     - A passive element which can be used with TABLE.INIT
    :DECLARATIVE - A forward declaration of table contents.

   INDEX is the index of the table to initialize. This slot is only
   used when MODE is :ACTIVE. By default this is 0, which is the only
   valid index in the current version of WebAssembly.

   OFFSET is an expression (list of instructions) which compute the
   starting index of the range to initialize. This field is only used
   when MODE is :ACTIVE.

   INIT is an object specifying the initial values of the table
   elements, which can be of the following can be either a
   `WASM-ELEMENT-INIT-INDEX' or `WASM-ELEMENT-INIT-EXPRESSIONS'
   object."

  (mode :active)
  (index 0)
  offset
  init)

(defstruct wasm-element-init-index
  "Represents a table element segment initialization with function indices.

   FUNCTIONS is a list of function indices to which the table elements
   are initialized, starting from the index OFFSET (of the WASM-ELEMENT object)."

  functions)

(defstruct wasm-element-init-expressions
  "Represents a table element segment initialization with expressions.

   TYPE is the table element type, either FUNCREF (default) or
   EXTERNREF. In the current version of WebAssembly only FUNCREF is
   supported.

   EXPRESSIONS is a list of expressions, where each expression is a
   list of instructions, which compute the initial values of the table
   elements, starting from the index OFFSET (of the WASM-ELEMENT object)."

  (type 'funcref)
  expressions)

(defstruct wasm-function
  "Represents a WebAssembly function.

   TYPE is the index of the functions type signature, within the type
   section.

   LOCALS is the list of the value types of the functions local
   variables.

   CODE is the list of instructions comprising the function."

  type
  locals
  code)

(defstruct wasm-data
  "Represents a data segment.

   Data segments specify the initial values of memory objects.

   MODE is a keyword specifying the data segment mode:

    :ACTIVE      - An active element initialized on module loading (the default)
    :PASSIVE     - A passive element which can be used with MEMORY.INIT

   MEMORY is the index of the memory object to initialize. This field
   is only used when MODE is :ACTIVE. By default this is 0, which is
   the only valid index in the current version of WebAssembly.

   OFFSET is an expression (list of instructions) which compute the
   starting index of the range to initialize. This field is only used
   when MODE is :ACTIVE.

   BYTES is the byte array containing the values to which the bytes,
   starting at OFFSET (in the case of an active data segment), are
   initialized."

  (mode :active)
  offset
  bytes
  (memory 0))

(defstruct (wasm-table (:include wasm-limit))
  "Represents a table object type.

   TYPE is the type of element stored in the table, either FUNCREF or
   EXTERNREF."

  (type 'funcref))

;;;; Value Types

(deftype u32 ()
  `(integer 0 ,(1- (expt 2 32))))

(deftype i32 ()
  `(integer ,(- (expt 2 31)) ,(1- (expt 2 31))))

(deftype u64 ()
  `(integer 0 ,(1- (expt 2 64))))

(deftype i64 ()
  `(integer ,(- (expt 2 63)) ,(1- (expt 2 63))))


;;; Module Serialization

(defun serialize-module (module stream)
  "Serialize a WebAssembly module to an output stream.

   MODULE is a `WASM-MODULE' object.

   STREAM is the output stream, which must be a binary stream with
   element type (unsigned-byte 8)."

  (write-sequence #(#x00 #x61 #x73 #x6D  ; Magic Number
                    #x01 #x00 #x00 #x00) ; Version Field
                  stream)

  (with-struct-slots wasm-module-
      (types imports functions tables memory globals exports start elements data data-count)
      module

    ;; Function Type Indices
    (serialize-types types stream)

    ;; Import Section
    (serialize-imports imports stream)

    ;; Function Type Signatures
    (serialize-function-types
     (map #'wasm-function-type functions)
     stream)

    ;; Table and Memory Sections
    (serialize-table-types tables stream)
    (serialize-memory-types memory stream)

    ;; Global Variable Section
    (serialize-global-section globals stream)

    ;; Exports Section
    (serialize-export-section exports stream)

    ;; Start Section
    (serialize-start-section start stream)

    ;; Indirect Function Table Elements
    (serialize-table-elements elements stream)

    ;; Add data count section
    (when (and data-count (not (emptyp data)))
      (serialize-data-count-section (length data) stream))

    ;; Function Code Section
    (serialize-functions functions stream)

    ;; Memory Data Section
    (serialize-data-section data stream)))


;;; Sections

;;;; Section Type Identifiers

(defconstant +custom-section-id+ 0
  "Custom section identifier.")

(defconstant +type-section-id+ 1
  "Function type signature section identifier.")

(defconstant +import-section-id+ 2
  "Import section identifier.")

(defconstant +function-section-id+ 3
  "Identifier of the section containing the types of the functions,
   in the module.")

(defconstant +table-section-id+ 4
  "Table section identifier.")

(defconstant +memory-section-id+ 5
  "Memory section identifier.")

(defconstant +global-section-id+ 6
  "Global variable section identifier.")

(defconstant +export-section-id+ 7
  "Export section identifier.")

(defconstant +start-section-id+ 8
  "Module entry point section identifier.")

(defconstant +element-section-id+ 9
  "Element section identifier.")

(defconstant +code-section-id+ 10
  "Function body section identifier.")

(defconstant +data-section-id+ 11
  "Data section identifier.")

(defconstant +data-count-section-id+ 12
  "Data count section identifier.")


;;;; Serialization Functions

(defun serialize-section (id fn stream)
  "Serialize a section, with identifier ID, to STREAM.

   FN is called with a single argument -- the stream to which the
   contents of the section should be written. The section id and the
   number of bytes are then written to STREAM followed by the bytes
   written by FN."

  (write-byte id stream)
  (serialize-with-byte-size fn stream))

(defun serialize-with-byte-size (fn stream)
  "Serialize a sequence of bytes to STREAM, preceded by the number of bytes.

   FN is called with a single argument -- the stream to which the
   contents of the section should be written. The number of bytes is
   then written to STREAM followed by the bytes written by FN."

  (let ((bytes
         (with-output-to-sequence (stream)
           (funcall fn stream))))

    (serialize-u32 (length bytes) stream)
    (write-sequence bytes stream)))


;;;; Function Type Section

(defun serialize-types (types stream)
  "Serialize the function type section consisting of the function type
   signatures, represented by WASM-FUNCTION-TYPE objects, in TYPES."

  (serialize-section
   +type-section-id+

   (lambda (stream)
     (serialize-vector #'serialize-ftype types stream))

   stream))


;;;; Imports Section

(defun serialize-imports (imports stream)
  "Serialize the import section, consisting of the import entries,
   represented by WASM-IMPORT objects, in IMPORTS."

  (serialize-section
   +import-section-id+

   (lambda (stream)
     (serialize-vector #'serialize-import imports stream))

   stream))

(defun serialize-import (import stream)
  "Serialize a single import entry IMPORT, represented by a
   WASM-IMPORT object."

  (check-type import wasm-import)

  (with-struct-slots wasm-import- (module name type desc)
      import

    (serialize-string module stream)
    (serialize-string name stream)

    (ecase type
      (:func
       (write-byte #x00 stream)
       (serialize-u32 desc stream))

      (:table
       (write-byte #x01 stream)
       (serialize-table desc stream))

      (:memory
       (write-byte #x02 stream)
       (serialize-memory desc stream))

      (:global
       (write-byte #x03 stream)
       (serialize-global (first desc) (second desc) stream)))))


;;;; Table and Memory Sections

(defun serialize-table-types (tables stream)
  "Serialize the table section, consisting of a table for each table
   limit, represented as a WASM-TABLE, given in TABLES."

  (serialize-section
   +table-section-id+

   (lambda (stream)
     (serialize-vector #'serialize-table tables stream))

   stream))

(defun serialize-memory-types (memory stream)
  "Serialize the memory section, consisting of a memory object for
   each limit, represented as a WASM-LIMIT, given in MEMORY."

  (serialize-section
   +memory-section-id+

   (lambda (stream)
     (serialize-vector #'serialize-memory memory stream))

   stream))


;;;; Global Variable Section

(defun serialize-global-section (globals stream)
  "Serialize the global variable section, consisting of the global
   variables, represented by WASM-GLOBAL objects, in GLOBALS."

  (flet ((serialize-global (global stream)
	   (check-type global wasm-global)

           (with-struct-slots wasm-global- (type mutable-p init)
               global

             (serialize-global type mutable-p stream)
             (serialize-expression init stream))))

    (serialize-section
     +global-section-id+

     (lambda (stream)
       (serialize-vector #'serialize-global globals stream))

     stream)))


;;;; Exports Section

(defun serialize-export-section (exports stream)
  "Serialize the export section consisting of the entries, represented
   by WASM-EXPORT objects, in EXPORTS."

  (serialize-section
   +export-section-id+

   (lambda (stream)
     (serialize-vector #'serialize-export exports stream))

   stream))

(defun serialize-export (export stream)
  "Serialize a single export entry, represented as a WASM-EXPORT
   object."

  (check-type export wasm-export)

  (with-struct-slots wasm-export- (name type index) export
    (serialize-string name stream)

    (write-byte
     (ecase type
       (:func #x00)
       (:table #x01)
       (:memory #x02)
       (:global #x03))
     stream)

    (serialize-u32 index stream)))


;;;; Start Section

(defun serialize-start-section (index stream)
  (when index
    (check-type index (integer 0))

    (serialize-section
     +start-section-id+

     (lambda (stream)
       (serialize-u32 index stream))

     stream)))


;;;; Table Element Section

(defun serialize-table-elements (elements stream)
  "Serialize the table element section, consisting of the table
   element initializations, represented by WASM-ELEMNT objects, in
   ELEMENTS."

  (serialize-section
   +element-section-id+

   (lambda (stream)
     (serialize-vector #'serialize-table-element elements stream))

   stream))

(defun serialize-table-element (table stream)
  "Serialize a single table element initialization entry, represented
   by a WASM-ELEMENT object."

  (check-type table wasm-element)

  (with-struct-slots wasm-element- (index mode offset init) table
    (check-type index (integer 0))
    (check-type mode (member :active :passive :declarative))
    (check-type init (or wasm-element-init-index wasm-element-init-expressions))

    ;; Serialize Type
    (write-byte (table-element-type-code table) stream)

    (when (= mode :active)
      (when (plusp index)
	(serialize-u32 index stream))

      (serialize-expression offset stream))

    (serialize-table-element-init mode index init stream)))

(defun serialize-table-element-init (mode index init stream)
  "Serialize a table element initializer.

   MODE is the table element mode.

   INDEX is the table index.

   INIT is the table element initializer.

   STREAM is the output stream"

  (cond
    ((wasm-element-init-index-p init)

     (when (or (/= mode :active) (plusp index))
       (write-byte #x00 stream))

     (serialize-vector #'serialize-u32 (wasm-element-init-index-functions init) stream))

    (t
     (when (or (/= mode :active)
	       (plusp index)
	       (/= 'funcref (intern-symbol (wasm-element-init-expressions-type init))))

       (serialize-ref-type (wasm-element-init-expressions-type init) stream))
     (serialize-vector #'serialize-expression (wasm-element-init-expressions-expressions init) stream))))

(defun table-element-type-code (element)
  "Return the type code for a table element.

   ELEMENT is the table element.

   Returns the type code as a byte."

  (with-struct-slots wasm-element- (index mode offset init)
      element

    (logior
     (if (member mode '(:passive :declarative)) 1 0)
     (if (or (= mode :declarative) (plusp index))
	 2 0)
     (if (wasm-element-init-expressions-p init) 4 0))))


;;;; Function Sections

(defun serialize-function-types (types stream)
  "Serialize the function type section (containing the indices of the
   type signatures of the functions defined in the module), consisting
   of the function type indices in TYPES."

  (serialize-section
   +function-section-id+

   (lambda (stream)
     (serialize-vector #'serialize-u32 types stream))

   stream))

(defun serialize-functions (functions stream)
  "Serialize the code section (containing the actual instructions
   comprising the functions in the module), consisting of the
   functions, represented by WASM-FUNCTION objects, in FUNCTIONS."

  (serialize-section
   +code-section-id+

   (lambda (stream)
     (serialize-vector #'serialize-function functions stream))

   stream))

(defun serialize-function (function stream)
  "Serialize the a single function, represented by a WASM-FUNCTION
   object. Only the local variable types and instructions of the
   function are serialized."

  (labels ((serialize-local (local stream)
             (destructuring-bind (type . count) local
               (serialize-u32 count stream)
               (serialize-type type stream)))

           (group-locals (locals)
             (let ((it (iterator locals)))
               (loop
                  until (endp it)
                  collect (group-next it))))

           (group-next (it)
             (loop
                with type = (at it)
                for count = 0 then (1+ count)
                until (or (endp it) (/= (at it) type))
                do (advance it)
                finally (return (cons type count)))))

    (check-type function wasm-function)

    (with-struct-slots wasm-function- (locals code)
        function

      (serialize-with-byte-size
       (lambda (stream)
         (serialize-vector #'serialize-local (group-locals locals) stream)
         (serialize-expression code stream))

       stream))))


;;; Data Section

(defun serialize-data-count-section (count stream)
  "Serialize the data count section.

   COUNT is the number of data segments.

   STREAM is the output stream."

  (serialize-section
   +data-count-section-id+

   (lambda (stream)
     (serialize-u32 count stream))

   stream))

(defun serialize-data-section (data stream)
  "Serialize the data section (containing the initialization of the
   module's memory), with the memory initialization
   entries (represented by WASM-DATA objects) in DATA."

  (serialize-section
   +data-section-id+

   (lambda (stream)
     (serialize-vector #'serialize-data data stream))

   stream))

(defun serialize-data (data stream)
  "Serialize a data segment."

  (check-type data wasm-data)

  (with-struct-slots wasm-data- (offset bytes memory mode)
      data

    (check-type memory (integer 0))
    (check-type mode (member :active :passive))

    (write-byte (data-segment-type-code data) stream)

    (when (= mode :active)
      (when (plusp memory) (serialize-u32 memory stream))
      (serialize-expression offset stream))

    (serialize-vector #'write-byte bytes stream)))

(defun data-segment-type-code (data)
  "Determine the type code of a data segment."

  (with-struct-slots wasm-data- (mode memory) data
    (logior
     (if (= mode :passive) 1 0)
     (if (plusp memory) 2 0))))


;;; Numbers

;;;; Floating Point

(defun serialize-float (value stream)
  "Serialize a single FLOAT to IEEE 32-bit floating point
   representation."

  (check-type value single-float)

  (serialize-32-bit (encode-float32 value) stream))

(defun serialize-double-float (value stream)
  "Serialize a DOUBLE-FLOAT to IEEE 64-bit floating point
   representation."

  (check-type value float)

  (serialize-64-bit (encode-float64 value) stream))


;;;; Integers

(defun serialize-u32 (value stream)
  (check-type value u32)

  (serialize-unsigned value stream))

(defun serialize-i32 (value stream)
  (check-type value i32)

  (serialize-signed value stream))


(defun serialize-unsigned (value stream)
  "Serialize an unsigned integer to LEB128 format"

  (check-type value (integer 0))

  (nlet encode ((n value))
    (let ((b (ldb (byte 7 0) n))
	  (n (ash n -7)))
      (cond
	((zerop n)
	 (write-byte b stream))

	(t
	 (write-byte (logior b #x80) stream)
	 (encode n))))))

(defun serialize-signed (value stream)
  "Serialize a signed integer to LEB128 format"

  (nlet encode ((n value))
    (let ((b (ldb (byte 7 0) n))
	  (n (ash n -7)))

      (cond
	((or (and (zerop n) (not (logbitp 6 b)))
	     (and (= n -1) (logbitp 6 b)))
	 (write-byte b stream))

	(t
	 (write-byte (logior b #x80) stream)
	 (encode n))))))


(defun serialize-32-bit (value stream)
  "Serialize an integer value to 4 bytes in little endian order."

  (check-type value integer)

  (write-byte (ldb (byte 8 0) value) stream)
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 16) value) stream)
  (write-byte (ldb (byte 8 24) value) stream))

(defun serialize-64-bit (value stream)
  "Serialize an integer value to 8 bytes in little endian order."

  (check-type value integer)

  (write-byte (ldb (byte 8 0) value) stream)
  (write-byte (ldb (byte 8 8) value) stream)
  (write-byte (ldb (byte 8 16) value) stream)
  (write-byte (ldb (byte 8 24) value) stream)

  (write-byte (ldb (byte 8 32) value) stream)
  (write-byte (ldb (byte 8 40) value) stream)
  (write-byte (ldb (byte 8 48) value) stream)
  (write-byte (ldb (byte 8 56) value) stream))


;;;; Strings

(defun serialize-string (value stream)
  "Serialize a string to a UTF-8 encoded string without BOM."

  (let ((octets (string-to-octets value :encoding :utf-8 :use-bom nil)))
    (serialize-u32 (length octets) stream)
    (write-sequence octets stream)))


;;; Vectors

(defun serialize-vector (fn vector stream)
  "Serialize a vector of values.

   The length VECTOR is written to STREAM first, after which FN is
   called on each element of the vector. FN should write each element
   it is called on to the stream passed to it in the second argument"

  (serialize-u32 (length vector) stream)
  (foreach (rcurry fn stream) vector))


;;; Types

(defun serialize-type (type stream)
  "Serialize a WebAssembly value type."

  (write-byte
   (ecase (intern-symbol type)
     (i32 #x7F)
     (i64 #x7E)
     (f32 #x7D)
     (f64 #x7C)

     (funcref #x70)
     (externref #x6F))

   stream))

(defun serialize-ref-type (type stream)
  "Serialize a WebAssembly function reference type."

  (write-byte
   (ecase (intern-symbol type)
     (funcref #x70)
     (externref #x6F))

   stream))

(defun serialize-ftype (type stream)
  "Serialize a function type signature, represented by a
   WASM-FUNCTION-TYPE object."

  (check-type type wasm-function-type)

  (with-struct-slots wasm-function-type- (params results)
      type

    (write-byte #x60 stream)
    (serialize-vector #'serialize-type params stream)
    (serialize-vector #'serialize-type results stream)))

(defun intern-symbol (sym)
  "Intern the symbol SYM in the package WASM-ENCODER."

  (intern (symbol-name sym) (find-package :wasm-encoder)))

(defun intern-symbols (syms)
  "If SYMS is a symbol intern it into WASM-ENCODER. If SYMS is a list,
   apply INTERN-SYMBOLS on each of element otherwise return SYMS as
   is."

  (match syms
    ((type list)
     (map #'intern-symbols syms))

    ((type symbol)
     (intern-symbol syms))

    (_ syms)))


;;; Memory and Table Declarations

(defun serialize-memory (limit stream)
  "Serialize a memory object entry with limit, represented by a
   WASM-LIMIT object, given by LIMIT."

  (serialize-limit limit stream))

(defun serialize-table (table stream)
  "Serialize a table object entry with limit, represented by a
   WASM-LIMIT object, given by LIMIT."

  (check-type table wasm-table)

  (serialize-ref-type (wasm-table-type table) stream)
  (serialize-limit table stream))

(defun serialize-limit (limit stream)
  "Serialize a memory/table limit represented by a WASM-LIMIT object."

  (check-type limit wasm-limit)

  (with-struct-slots wasm-limit- (min max) limit
    (cond
      (max
       (write-byte #x01 stream)
       (serialize-u32 min stream)
       (serialize-u32 max stream))

      (t
       (write-byte #x00 stream)
       (serialize-u32 min stream)))))


;;; Global Variables

(defun serialize-global (type mutable? stream)
  "Serialize a global variable with value type TYPE. If MUTABLE? is
   true a mutable global variable is serialized."

  (serialize-type type stream)
  (write-byte
   (if mutable? #x01 #x00)
   stream))


;;; Instructions

(defconstant +op-codes+
  '((unreachable . #x00)
    (nop . #x01)

    (block . #x02)
    (loop . #x03)
    (if . #x04)

    (br . #x0C)
    (br_if . #x0D)
    (br_table . #x0E)
    (return . #x0F)
    (call . #x10)
    (call_indirect . #x11)

    (drop . #x1A)
    (select . #x1B)
    ((select) . #x1C)

    (local.get . #x20)
    (local.set . #x21)
    (local.tee . #x22)
    (global.get . #x23)
    (global.set . #x24)

    (table.get . #x25)
    (table.set . #x26)

    (i32.load . #x28)
    (i64.load . #x29)
    (f32.load . #x2A)
    (f64.load . #x2B)

    (i32.load8_s . #x2C)
    (i32.load8_u . #x2D)
    (i32.load16_s . #x2E)
    (i32.load16_u . #x2F)

    (i64.load8_s . #x30)
    (i64.load8_u . #x31)
    (i64.load16_s . #x32)
    (i64.load16_u . #x33)
    (i64.load32_s . #x34)
    (i64.load32_u . #x35)

    (i32.store . #x36)
    (i64.store . #x37)
    (f32.store . #x38)
    (f64.store . #x39)

    (i32.store8 . #x3A)
    (i32.store16 . #x3B)
    (i64.store8 . #x3C)
    (i64.store16 . #x3D)
    (i64.store32 . #x3E)

    (memory.size . #x3F)
    (memory.grow . #x40)

    (i32.const . #x41)
    (i64.const . #x42)
    (f32.const . #x43)
    (f64.const . #x44)

    (i32.eqz . #x45)
    (i32.eq . #x46)
    (i32.ne . #x47)
    (i32.lt_s . #x48)
    (i32.lt_u . #x49)
    (i32.gt_s . #x4A)
    (i32.gt_u . #x4B)
    (i32.le_s . #x4C)
    (i32.le_u . #x4D)
    (i32.ge_s . #x4E)
    (i32.ge_u . #x4F)

    (i64.eqz . #x50)
    (i64.eq . #x51)
    (i64.ne . #x52)
    (i64.lt_s . #x53)
    (i64.lt_u . #x54)
    (i64.gt_s . #x55)
    (i64.gt_u . #x56)
    (i64.le_s . #x57)
    (i64.le_u . #x58)
    (i64.ge_s . #x59)
    (i64.ge_u . #x5A)

    (f32.eq . #x5B)
    (f32.ne . #x5C)
    (f32.lt . #x5D)
    (f32.gt . #x5E)
    (f32.le . #x5F)
    (f32.ge . #x60)

    (f64.eq . #x61)
    (f64.ne . #x62)
    (f64.lt . #x63)
    (f64.gt . #x64)
    (f64.le . #x65)
    (f64.ge . #x66)

    (i32.clz . #x67)
    (i32.ctz . #x68)
    (i32.popcnt . #x69)
    (i32.add . #x6A)
    (i32.sub . #x6B)
    (i32.mul . #x6C)
    (i32.div_s . #x6D)
    (i32.div_u . #x6E)
    (i32.rem_s . #x6F)
    (i32.rem_u . #x70)
    (i32.and . #x71)
    (i32.or . #x72)
    (i32.xor . #x73)
    (i32.shl . #x74)
    (i32.shr_s . #x75)
    (i32.shr_u . #x76)
    (i32.rotl . #x77)
    (i32.rotr . #x78)

    (i64.clz . #x79)
    (i64.ctz . #x7A)
    (i64.popcnt . #x7B)
    (i64.add . #x7C)
    (i64.sub . #x7D)
    (i64.mul . #x7E)
    (i64.div_s . #x7F)
    (i64.div_u . #x80)
    (i64.rem_s . #x81)
    (i64.rem_u . #x82)
    (i64.and . #x83)
    (i64.or . #x84)
    (i64.xor . #x85)
    (i64.shl . #x86)
    (i64.shr_s . #x87)
    (i64.shr_u . #x88)
    (i64.rotl . #x89)
    (i64.rotr . #x8A)

    (f32.abs . #x8B)
    (f32.neg . #x8C)
    (f32.ceil . #x8D)
    (f32.floor . #x8E)
    (f32.trunc . #x8F)
    (f32.nearest . #x90)
    (f32.sqrt . #x91)
    (f32.add . #x92)
    (f32.sub . #x93)
    (f32.mul . #x94)
    (f32.div . #x95)
    (f32.min . #x96)
    (f32.max . #x97)
    (f32.copysign . #x98)

    (f64.abs . #x99)
    (f64.neg . #x9A)
    (f64.ceil . #x9B)
    (f64.trunc . #x9C)
    (f64.floor . #x9D)
    (f64.nearest . #x9E)
    (f64.sqrt . #x9F)
    (f64.add . #xA0)
    (f64.sub . #xA1)
    (f64.mul . #xA2)
    (f64.div . #xA3)
    (f64.min . #xA4)
    (f64.max . #xA5)
    (f64.copysign . #xA6)

    (i32.wrap_i64 . #xA7)
    (i32.trunc_f32_s . #xA8)
    (i32.trunc_f32_u . #xA9)
    (i32.trunc_f64_s . #xAA)
    (i32.trunc_f64_u . #xAB)
    (i64.extend_i32_s . #xAC)
    (i64.extend_i32_u . #xAD)
    (i64.trunc_f32_s . #xAE)
    (i64.trunc_f32_u . #xAF)
    (i64.trunc_f64_s . #xB0)
    (i64.trunc_f64_u . #xB1)

    (f32.convert_i32_s . #xB2)
    (f32.convert_i32_u . #xB3)
    (f32.convert_i64_s . #xB4)
    (f32.convert_i64_u . #xB5)
    (f32.demote_f64 . #xB6)

    (f64.convert_i32_s . #xB7)
    (f64.convert_i32_u . #xB8)
    (f64.convert_i64_s . #xB9)
    (f64.convert_i64_u . #xBA)
    (f64.promote_f64 . #xBB)

    (i32.reinterpret_f32 . #xBC)
    (i64.reinterpret_f64 . #xBD)
    (f32.reinterpret_i32 . #xBE)
    (f64.reinterpret_i64 . #xBF)

    (i32.extend8_s . #xC0)
    (i32.extend16_s . #xC1)
    (i64.extend8_s . #xC2)
    (i64.extend16_s . #xC3)
    (i64.extend32_s . #xC4)

    (ref.null . #xD0)
    (ref.is_null . #xD1)
    (ref.func . #xD2)

    (i32.trunc_sat_f32_s . (#xFC #X00))
    (i32.trunc_sat_f32_u . (#xFC #X01))
    (i32.trunc_sat_f64_s . (#xFC #X02))
    (i32.trunc_sat_f64_u . (#xFC #X03))
    (i64.trunc_sat_f32_s . (#xFC #X04))
    (i64.trunc_sat_f32_u . (#xFC #X05))
    (i64.trunc_sat_f64_s . (#xFC #X06))
    (i64.trunc_sat_f64_u . (#xFC #X07))

    (memory.init . (#xFC #x08))
    (data.drop . (#xFC #x09))
    (memory.copy . (#xFC #x0A))
    (memory.fill . (#xFC #x0B))

    (table.init . (#xFC #x0C))
    (elem.drop . (#xFC #x0D))
    (table.copy . (#xFC #x0E))
    (table.grow . (#xFC #x0F))
    (table.size . (#xFC #x10))
    (table.fill . (#xFC #x11)))

  "WebAssembly Instruction Opcode Map.")

(defun serialize-instructions (instructions stream)
  "Serialize the instructions, in INSTRUCTIONS, to STREAM."

  (foreach (rcurry #'serialize-instruction stream) instructions))

(defun serialize-instruction (instruction stream)
  "Serialize a WebAssembly instruction to STREAM. The instruction may
   either be a symbol, naming the instruction, or a list in which the
   first element is the symbol naming the instruction and the
   remaining elements are the instruction operands."

  (match instruction
    ((or (list* op operands) op)

     (let* ((op (intern-symbol op))
	    (opcode (or
		     (when (listp instruction) (get (list op) +op-codes+))
		     (get op +op-codes+))))

       (assert opcode (opcode) "Unknown instruction ~a" op)

       (write-sequence (ensure-list opcode) stream)
       (serialize-instruction-operands op (intern-symbols operands) stream)))))

(defgeneric serialize-instruction-operands (op operands stream)
  (:documentation
   "Serialize the operands (OPERANDS) of the instruction OP."))

(defmethod serialize-instruction-operands ((op t) operands stream)
  "Default Method, does nothing."

  (declare (ignore operands stream))
  (assert (null operands) (operands) "Instruction ~a does not take immediate operands" op)
  nil)

;;;; Blocks

(defmethod serialize-instruction-operands ((op (eql 'block)) body stream)
  (serialize-block body stream))

(defmethod serialize-instruction-operands ((op (eql 'loop)) body stream)
  (serialize-block body stream))


(defun serialize-block (body stream)
  "Serialize a structured block instruction containing the
   instructions in BODY."

  (match body
    ((list* (list 'result type) body)
     (serialize-type type stream)
     (serialize-instructions body stream))

    ((list* (list 'type index) body)
     (check-type index (integer 0))
     (serialize-signed index stream)
     (serialize-instructions body stream))

    (_
     (serialize-nil-type stream)
     (serialize-instructions body stream)))

  (write-byte #x0B stream))

(defun serialize-nil-type (stream)
  (write-byte #x40 stream))


;;;; Parametric Instructions

(defmethod serialize-instruction-operands ((op (eql 'select)) operands stream)
  "Serialize the value types of the a select t* instruction"

  (unless (emptyp operands)
    (serialize-vector #'serialize-type operands stream)))


;;;; If/Then/Else Blocks

(defmethod serialize-instruction-operands ((op (eql 'if)) body stream)
  "Serialize the body of an IF structured instruction."

  (flet ((serialize-body (body)
           (ematch body
             ((list (list* 'then then)
                    (list* 'else else))

              (serialize-instructions then stream)
              (write-byte #x05 stream)
              (serialize-instructions else stream))

             ((list (list* 'then then))
              (serialize-instructions then stream)))))

    (match body
      ((list* (list 'result type) body)
       (serialize-type type stream)
       (serialize-body body))

      ((list* (list 'type index) body)
       (check-type index (integer 0))
       (serialize-signed index stream)
       (serialize-body body))

      (_
       (serialize-nil-type stream)
       (serialize-body body)))

    (write-byte #x0B stream)))


;;;; Branch Instructions

(defmethod serialize-instruction-operands ((op (eql 'br)) operands stream)
  (serialize-branch operands stream))

(defmethod serialize-instruction-operands ((op (eql 'br_if)) operands stream)
  (serialize-branch operands stream))

(defmethod serialize-instruction-operands ((op (eql 'call)) operands stream)
  (serialize-branch operands stream))

(defun serialize-branch (operands stream)
  "Serialize the branch index of a branch instruction."

  (destructuring-bind (index) operands
    (check-type index (integer 0))

    (serialize-u32 index stream)))


(defmethod serialize-instruction-operands ((op (eql 'br_table)) operands stream)
  "Serialize the branches indices of a branch table instruction."

  (assert (not (emptyp operands)))

  (serialize-u32 (1- (length operands)) stream)
  (foreach (rcurry #'serialize-u32 stream) operands))


;;;; Call Indirect

(defmethod serialize-instruction-operands ((op (eql 'call_indirect)) operands stream)
  "Serialize the type signature of a call indirect instruction."

  (destructuring-bind (type &optional (table 0)) operands
    (check-type type (integer 0))

    (serialize-u32 type stream)
    (write-byte table stream)))


;;;; Variables

(defmethod serialize-instruction-operands ((op (eql 'local.get)) operands stream)
  (serialize-variable-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'local.set)) operands stream)
  (serialize-variable-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'local.tee)) operands stream)
  (serialize-variable-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'global.get)) operands stream)
  (serialize-variable-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'global.set)) operands stream)
  (serialize-variable-instruction operands stream))

(defmethod serialize-variable-instruction (operands stream)
  "Serialize the local variable index of a local variable
   instruction."

  (destructuring-bind (index) operands
    (check-type index (integer 0))

    (serialize-u32 index stream)))


;;;; Tables

(defmethod serialize-instruction-operands ((op (eql 'table.get)) operands stream)
  (serialize-table-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'table.set)) operands stream)
  (serialize-table-instruction operands stream))

(defmethod serialize-table-instruction (operands stream)
  "Serialize the table index of a table instruction."

  (destructuring-bind (index) operands
    (check-type index (integer 0))

    (serialize-u32 index stream)))

(defmethod serialize-instruction-operands ((op (eql 'table.init)) operands stream)
  (destructuring-bind (table elem) operands
    (check-type table (integer 0))
    (check-type elem (integer 0))

    (serialize-u32 elem stream)
    (serialize-u32 table stream)))

(defmethod serialize-instruction-operands ((op (eql 'table.copy)) operands stream)
  (destructuring-bind (elem table) operands
    (check-type elem (integer 0))
    (check-type table (integer 0))

    (serialize-u32 elem stream)
    (serialize-u32 table stream)))

(defmethod serialize-instruction-operands ((op (eql 'elem.drop)) operands stream)
  (serialize-table-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'table.grow)) operands stream)
  (serialize-table-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'table.size)) operands stream)
  (serialize-table-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'table.fill)) operands stream)
  (serialize-table-instruction operands stream))

;;;; Memory Load and Store

(defmethod serialize-instruction-operands ((op (eql 'i32.load)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.load)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'f32.load)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'f64.load)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i32.load8_s)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i32.load8_u)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i32.load16_s)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i32.load16_u)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.load8_s)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.load8_u)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.load16_s)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.load16_u)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.load32_s)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.load32_u)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i32.store)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.store)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'f32.store)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'f64.store)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i32.store8)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i32.store16)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.store8)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.store16)) operands stream)
  (serialize-memory-instruction operands stream))

(defmethod serialize-instruction-operands ((op (eql 'i64.store32)) operands stream)
  (serialize-memory-instruction operands stream))

(defun serialize-memory-instruction (operands stream)
  "Serialize the offset and alignment operands of a memory load/store
   instruction."

  (let ((alignment (or (second (assoc 'align operands)) 2))
        (offset (or (second (assoc 'offset operands)) 0)))

    (check-type alignment (integer 0))
    (check-type offset (integer 0))

    (serialize-u32 alignment stream)
    (serialize-u32 offset stream)))


;;;; Memory Pages

(defmethod serialize-instruction-operands ((op (eql 'memory.size)) operands stream)
  (assert (null operands))
  (write-byte #x00 stream))

(defmethod serialize-instruction-operands ((op (eql 'memory.grow)) operands stream)
  (assert (null operands))
  (write-byte #x00 stream))

(defmethod serialize-instruction-operands ((op (eql 'memory.copy)) operands stream)
  (assert (null operands))
  (write-sequence #(#x00 #x00) stream))

(defmethod serialize-instruction-operands ((op (eql 'memory.fill)) operands stream)
  (assert (null operands))
  (write-byte #x00 stream))

(defmethod serialize-instruction-operands ((op (eql 'memory.init)) operands stream)
  (destructuring-bind (dataidx) operands
    (check-type dataidx (integer 0))
    (serialize-u32 dataidx stream)
    (write-byte #x00 stream)))

(defmethod serialize-instruction-operands ((op (eql 'data.drop)) operands stream)
  (destructuring-bind (dataidx) operands
    (check-type dataidx (integer 0))
    (serialize-u32 dataidx stream)))

;;; Constants

(defmethod serialize-instruction-operands ((op (eql 'i32.const)) operands stream)
  (destructuring-bind (constant) operands
    (etypecase constant
      (i32 (serialize-i32 constant stream))
      (u32 (serialize-signed (logior (- (expt 2 32)) constant) stream)))))

(defmethod serialize-instruction-operands ((op (eql 'i64.const)) operands stream)
  (destructuring-bind (constant) operands
    (etypecase constant
      (i64 (serialize-signed constant stream))
      (u64 (serialize-signed (logior (- (expt 2 64)) constant) stream)))))

(defmethod serialize-instruction-operands ((op (eql 'f32.const)) operands stream)
  (destructuring-bind (constant) operands
    (serialize-float constant stream)))

(defmethod serialize-instruction-operands ((op (eql 'f64.const)) operands stream)
  (destructuring-bind (constant) operands
    (serialize-double-float constant stream)))


;;; Reference Instructions

(defmethod serialize-instruction-operands ((op (eql 'ref.null)) operands stream)
  (destructuring-bind (type) operands
    (serialize-ref-type type stream)))

(defmethod serialize-instruction-operands ((op (eql 'ref.func)) operands stream)
  (destructuring-bind (index) operands
    (serialize-u32 index stream)))


;;; Expressions

(defun serialize-expression (instructions stream)
  "Serialize a list of instructions (an expression) followed by the
   block end instruction."

  (serialize-instructions instructions stream)
  (write-byte #x0B stream))
