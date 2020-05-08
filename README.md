# wasm-encoder

This is a simple library for serializing WebAssembly modules to binary
`.wasm` files.

## Documentation

### Modules

A WebAssembly module is represented by the `WASM-MODULE` structure,
which contains a slot for each section.

Slots:

 - `TYPES`
 - `IMPORTS`
 - `FUNCTIONS`
 - `TABLES`
 - `MEMORY`
 - `GLOBALS`
 - `EXPORTS`
 - `START`
 - `ELEMENTS`
 - `DATA`

### Type Section

The `TYPES` slot contains the list of the function types comprising the
type section of the module. Each type is represented by an instance of
the `WASM-FUNCTION-TYPE` structure, which contains the following
slots:

 <dl>
  <dt>`PARAMS`</dt>
  <dd>List of parameter types</dd>
  <dt>`RESULTS`</dt>
  <dd>List of result types</dd>
 </dl>

**NOTE:** In its current form, WebAssembly only supports functions
with a maximum of one return value, however the binary format allows
for function types with more than a single return value. This library
supports function types with more than a single return value.


#### Type Identifiers

Each element of `PARAMS` and `RESULTS` is one of the following type
identifier symbols:

 <dl>
  <dt>`I32`</dt>
  <dd>32-bit Integer</dd>
  <dt>`I64`</dt>
  <dd>64-bit Integer</dd>

  <dt>`F32`</dt>
  <dd>32-bit Single Precision Floating Point</dd>
  <dt>`F64`</dt>
  <dd>64-bit Double Precision Floating Point</dd>
 </dl>

Any symbol, in any package, of which the symbol name is equal to one
of the above is accepted.


### Imports Section

The `IMPORTS` slot contains the list of the module's imports. Each
import is represented by an instance of the `WASM-IMPORT` structure,
which has the following slots:

 <dl>
  <dt>`MODULE`</dt>
  <dd>Module component (first level) of the import name</dd>

  <dt>`NAME`</dt>
  <dd>Name component (second level) of the import name</dd>

  <dt>`TYPE`</dt>
  <dd>Type of the imported entity</dd>

  <dt>`DESC`</dt>
  <dd>Description of the entity, depends on `TYPE`.</dd>
 </dl>

`DESC` may be one of the following keywords indicating the type of the
imported entity:

`:FUNC` -- The imported entity is a function with `DESC` interpreted
as the index of the function type which describes the imported
function's signature.

`:TABLE` -- The imported entity is a table with `DESC` expected to be
a `WASM-LIMIT` object containing the minimum and maximum size of the
table.

`:MEMORY` -- The imported entity is a memory object with `DESC`
expected to be a `WASM-LIMIT` object containing the minimum and
maximum memory size.

`:GLOBAL` -- The imported entity is a global variable with `DESC`
expected to be a list of two elements where the first element is the
global variable type identifier (one of `I32`, `I64`, `F32`, `F64`),
and the second element is interpreted as a Boolean flag indicating
whether the variable is mutable (true) or not.


### Functions

The slot `FUNCTIONS` contains the list of the functions making up the
module. Each function is represented by an instance of the
`WASM-FUNCTION` structure, which has the following slots:

 <dl>
  <dt>`TYPE`</dt>
  <dd>Function type index.</dd>

  <dt>`LOCALS`</dt>
  <dd>
  List of the local variable type identifiers. Each element should
  be one of the symbols listed in [Type Identifiers](#type-identifiers).
  </dd>

  <dt>`CODE`</dt> <dd>List of the instructions comprising the body of
  the function.</dd>
 </dl>

### Table and Memory Sections

The slot `TABLES` contains the list of the limits for each table
object. Similarly the `MEMORY` slot contains the list of the limits
for each memory object.

**NOTE:** WebAssembly currently only allows a single table and memory
object per module.

Each limit is represented by an instance of the `WASM-LIMIT` structure
which has two slots:

 <dl>
  <dt>`MIN`</dt>
  <dd>The lower-bound of the limit.</dd>

  <dt>`MAX`</dt>
  <dd> The upper-bound of the limit. This may be `NIL` to indicate
  that there is no upper-bound.  </dd>
 </dl>

For table objects the limit specifies the minimum and maximum, number
of elements in the table. For memory object the limit specifies the
minimum and maximum number of pages, with each page being 64 kilobytes
in size.


### Global Variable Section

The `GLOBALS` slot contains the list of the module's global
variables. Each global variable is represented by an instance of the
`WASM-GLOBAL` structure, which has the following slots:

 <dl>
  <dt>`TYPE`</dt>
  <dd>Global variable type identifier, which should one of the type
  identifiers symbols listed in [Type
  Identifiers](#type-identifiers)</dd>

  <dt>`MUTABLE-P`</dt>
  <dd>If true indicates that the variable is mutable otherwise if
  `NIL` the variable is immutable.  </dd>

  <dt>`INIT`</dt>
  <dd>List of instructions which compute the variable's initial
  value. May be `NIL`</dd>

 </dl>

### Exports Section

The `EXPORTS` slot contains the list of the module's exports. Each
export is represented by an instance of the `WASM_EXPORT` structure,
which has the following slots:

 <dl>
  <dt>`NAME`</dt>
  <dd>The name (as a string) under which the entity is exported.</dd>

  <dt>`TYPE`</dt>
  <dd>The type of entity which is being exported. This should be one
  of the keyword symbols listed in [Imports
  Section](#imports-section)</dd>

  <dt>`INDEX`</dt>
  <dd>Index of the exported entity.</dd>
 </dl>


### Start Function

The `START` slot contains the index of the function which serves as
the module's entry point, which is executed as soon as the module is
instantiated. This may be `NIL` in which the module does not have an
entry point.


### Table Elements Section

The `ELEMENTS` slot contains the table initialization of the
module. This is a list of `WASM-TABLE` objects, which have the
following slots:

 <dl>
  <dt>`INDEX`</dt>
  <dd>Index of the table which is being initialized. *Currently
  WebAssembly only supports a single table at index 0.*</dd>

  <dt>`OFFSET`</dt>
  <dd>Expression (list of instructions) which compute the offset of
  the first table element to which is being initialized.</dd>

  <dt>`FUNCTIONS`</dt>
  <dd>List of function indices to which the table elements, starting
  from the index computed by `OFFSET`, are initialized.</dd>
 </dl>


### Data Section

The `DATA` slot contains the list of data segments which initialize
the module's memory. Each data segment is represented by an instance
of the `WASM-DATA` structure, which has the following slots:

 <dl>
  <dt>`OFFSET`</dt>
  <dd>Expression (list of instructions) which compute the address of
  the memory byte to initialize.</dd>

  <dt>`BYTES`</dt>
  <dd>Sequence of bytes `(UNSIGNED-BYTE 8)` to which the segment of
  memory, starting at the address computed by `OFFSET`, is
  initialized.</dd>

  <dt>`MEMORY`</dt>
  <dd>Index of the memory object which is being
  initialized. *Currently WebAssembly only supports a single memory
  object at index 0.*</dd>
 </dl>


