//! # LuaJIT bytecode module
//!
//! This module contains all required information and data to produce valid
//! LuaJIT bytecode buffers (version 2).
//!
//! A LuaJIT bytecode buffer has the following structure:
//!   * A header containing information about the execution context
//!   * Multiple function prototypes
//!   * One byte set to `0` to flag the buffer end
//!
//! The last prototype is considered as the "main" function and is
//! automatically executed, it must be variadic.
//!
//! ## The header part
//!
//! A LuaJIT bytecode buffer header part is defined like this:
//!
//! ```no_run
//! MAGIC (3 byte) = [0x1B, 0x4C, 0x4A]
//! VERSION (1 byte) = 0x01 | 0x02
//! FLAGS (1 byte) = {
//!     0b00000001 = IS_BIG_ENDIAN
//!     0b00000010 = IS_STRIPPED
//!     0b00000100 = HAS_FFI_ACCESS
//!     0b00001000 = ???
//! }
//! (
//!     SOURCE_NAME_LEN (1 uleb128)
//!     SOURCE_NAME (byte[SOURCE_NAME_LEN])
//! )? if !IS_STRIPPED
//! ```
//!
//! ## Prototypes
//!
//! Here is the structure of a function prototype in LuaJIT bytecode format:
//!
//! ```no_run
//! SIZE (1 uleb128)
//! FLAGS (1 byte) = {
//!     0b00000001 = HAS_CHILD
//!     0b00000010 = IS_VARIADIC
//!     0b00000100 = HAS_FFI
//!     0b00001000 = JIT_DISABLED
//!     0b00010000 = HAS_ILOOP
//! }
//! ARG_COUNT (1 byte)
//! FRAME_SIZE (1 byte)
//! UPVALUE_CONST_COUNT (1 byte)
//! COMPLEX_CONST_COUNT (1 uleb128)
//! NUM_CONST_COUNT (1 uleb128)
//! INST_COUNT (1 uleb128)
//! (
//!     DEBUG_INFO_SIZE (1 uleb128)
//!     FIRST_LINE_NB (1 uleb128)
//!     LINE_COUNT (1 uleb128)
//! )? if !IS_STRIPPED
//! INSTRUCTIONS (1 int32[INST_COUNT])
//! CONSTANT_TABLE
//! ```
//!
//! With:
//!   * `SIZE` -> The size of the whole function prototype, without the `SIZE`
//!     field
//!   * `FRAME_SIZE` -> The number of local slots used during the function
//!     execution
//!
//! When a function is called, arguments are placed in first slots of the
//! frame, following their declaration order. Example:
//!
//! ```lua
//! function test(a, b) end
//! ```
//!
//! In the bytecode representation of the previous Lua code, `a` variable is
//! stored in the local slot `0` and `b` in the `1`.
//!
//! ## The constant table
//!
//! `CONSTANT_TABLE` is a section at the end of every function prototype which
//! contains all constants used in the function. Its structure is:
//!
//! ### Up-value constants
//!
//! ```no_run
//! UPVALUE_CONSTS (1 short16[UPVAL_COUNT])
//! COMPLEX_CONSTS (1 COMPLEX_CONST[COMPLEX_CONST_COUNT])
//! NUMERIC_CONSTS (1 NUMERIC_CONST[NUM_CONST_COUNT])
//! ```
//!
//! An "up-value" (shortened "UV") is an index that correspond to a local slot
//! or an UV constant in the parent scope that is referenced in the function.
//! This is used to encode the "closure" concept in LuaJIT.
//! An UV constant is a 2-byte value representing an index and a flag. The two
//! highest bits tells whether the index is about a local variable or another
//! UV in the parent scope. Example:
//!   * `0b1100000000000001` -> Enclose the local slot in the parent scope with
//!     index "1"
//!   * `0b0000000000000001` -> Enclose the UV constant in the parent scope
//!     with index "1"
//!
//! ### Complex constants
//!
//! A `COMPLEX_CONST` in the LuaJIT bytecode is composed of two parts:
//!
//! ```no_run
//! CONSTANT_KIND (1 uleb128) = {
//!     0 = CHILD
//!     1 = TABLE
//!     2 = I64
//!     3 = U64
//!     4 = COMPLEX
//!     5 + STR_LEN = STR
//! }
//! CONSTANT_DATA (~ depending on CONSTANT_KIND)
//! ```
//!
//! With:
//!   * `CHILD` -> A function prototype, child of the current prototype
//!
//! #### Table constants
//!
//! Table constants are structured as specified bellow:
//!
//! ```no_run
//! ARRAY_ITEM_COUNT (1 uleb128)
//! HASH_ITEM_COUNT (1 uleb128)
//! ARRAY (TABLE_CONSTANT[ARRAY_ITEM_COUNT])
//! MAP ((TABLE_CONSTANT, TABLE_CONSTANT)[HASH_ITEM_COUNT])
//! ```
//!
//! **IMPORTANT:** Since Lua table are 1-indexed, if a table constant owns array
//! items, `nil` value inserted at the first position and counted in the
//! `ARRAY_ITEM_COUNT` value.
//!
//! A `TABLE_CONSTANT` is a value that follow the same principle as
//! `COMPLEX_CONSTANT`: an ULEB128 as kind with a constant data which size
//! is depending of the type, as the following description shows:
//!
//! ```no_run
//! TABLE_CONSTANT_KIND (1 uleb128) = {
//!     0 = nil
//!     1 = false
//!     2 = true
//!     3 = INTEGER
//!     4 = NUM
//!     5 = STRING
//! }
//! TABLE_CONSTANT_DATA (~ depending on TABLE_CONSTANT_KIND)
//! ```
//!
//! Here are the encoding of `TABLE_CONSTANT_DATA` in function of the
//! associated `TABLE_CONSTANT_KIND`:
//!   * If `TABLE_CONSTANT_KIND` is `0`, `1` or `2` -> There is no
//!     `TABLE_CONSTANT_DATA` field
//!   * If `TABLE_CONSTANT_KIND` is `3` -> The data is a ULEB128 representing
//!     the value of the i32 encoded integer
//!   * If `TABLE_CONSTANT_KIND` is `4` -> The data field is a float64 encoded
//!     as 2 ULEB128 numbers, first one being the low part of the number (32
//!     lease significant bits), second one representing the high part (32
//!     other bits
//!   * If `TABLE_CONSTANT_KIND` is `5` or more -> The data represents a string
//!     and follows the same rules as [string constants](#string-constants)
//!
//! #### Integer constants
//!
//! I64 and U64 constants represents simple integers, they are encoded in the
//! ULEB128 format.
//!
//! #### Complex constants
//!
//! **TODO**
//!
//! #### String constants
//!
//! A string constant is identified by a kind greater or equals to 5. Since the
//! kind is an ULEB128 value, it also contains the length of the following
//! string. Example:
//!   * `0x08 0x66 0x6F 0x6F` -> Represents the string constant "foo"
//!
//! ### Numeric constants
//!
//! In the LuaJIT bytecode format, a numeric constant may represent either an
//! integer or a number value. A number may be a decimal value, thus we need to
//! represents this decimal part.
//!
//! In order to encode those possibilities, in the first element of the ULEB128
//! representation of the value, the least significant bit tells whether the
//! numeric constant is an integer or a float value. Example:
//!   * `0b00000010` -> Means that the value is an integer valued to "1"
//!     (`0000001` == `1`)
//!   * `0b00000011 0b00000101` -> Means that the value is a decimal with the
//!     low part first, then the high part
//!
//! Integer values are represented using signed 32 bits integers, if the
//! constant value cannot be represented in it, it becomes a float64 decimal
//! constant.
//!
//! Decimal numbers are represented using float64 encoded values, their
//! representation in the bytecode buffer is done as described bellow:
//!
//! ```no_run
//! LOW_PART = (1 uleb128)  // The 32 least significant bits of the f64 with
//!                         // the numeric constant flag on the first byte
//! HIGH_PART = (1 uleb128) // The 32 most significant bits of the f64
//! ```

mod op_codes;

// ----- Header related constants -----

pub const MAGIC: [u8; 3] = [0x1B, 0x4C, 0x4A];
pub const CUR_VERSION: u8 = 0x02;
pub const MAX_VERSION: u8 = 0x80;

pub const FLAG_H_IS_BIG_ENDIAN: u8 = 0b00000001; // Whether the bytecode is encoded in big endian
pub const FLAG_H_IS_STRIPPED: u8 = 0b00000010; // Whether the the bytecode is stripped (without debug info)
pub const FLAG_H_HAS_FFI: u8 = 0b00000100; // Whether the bytecode has FFI access
pub const FLAG_H_FR2: u8 = 0b00001000; // ???

// ----- Prototype related constants -----

pub const FLAG_P_HAS_CHILD: u8 = 0b00000001;
pub const FLAG_P_IS_VARIADIC: u8 = 0b00000010;
pub const FLAG_P_HAS_FFI: u8 = 0b00000100;
pub const FLAG_P_JIT_DISABLED: u8 = 0b00001000;
pub const FLAG_P_HAS_ILOOP: u8 = 0b00010000;

pub const JUMP_BIASING: u16 = 0x8000;

// ----- Complex constant kinds -----

pub const COMPLEX_CONST_CHILD_KIND: u8 = 0;
pub const COMPLEX_CONST_TAB_KIND: u8 = 1;
pub const COMPLEX_CONST_I64_KIND: u8 = 2;
pub const COMPLEX_CONST_U64_KIND: u8 = 3;
pub const COMPLEX_CONST_COMPLEX_NUM_KIND: u8 = 4;
pub const COMPLEX_CONST_STR_KIND: u8 = 5;

// ----- Table constant kinds -----

pub const TABLE_CONST_NIL_KIND: u8 = 0;
pub const TABLE_CONST_FALSE_KIND: u8 = 1;
pub const TABLE_CONST_TRUE_KIND: u8 = 2;
pub const TABLE_CONST_INT_KIND: u8 = 3;
pub const TABLE_CONST_NUM_KIND: u8 = 4;
pub const TABLE_CONST_STR_KIND: u8 = 5;

// ----- Bytecode emission helpers -----

/// This structure represents an executable bytecode buffer, structured as
/// specified in the [`bytecode`](mod@self) module.
#[derive(Debug)]
pub struct BytecodeBuffer {
    // Function prototypes, the last one is considered as the main
    pub prototypes: Vec<Prototype>,
}

/// This structure represents a function prototype in the LuaJIT bytecode
/// encoding.
#[derive(Debug)]
pub struct Prototype {
    // --- Flags
    pub has_child: bool,
    pub is_variadic: bool,
    pub has_ffi: bool,

    // --- Function specification
    pub arg_count: u8,
    pub frame_size: u8,

    // --- Instructions
    pub instructions: Vec<Instruction>,

    // --- Constants
    pub up_values: Vec<UpValueConstant>,
    pub complex_consts: Vec<ComplexConstant>,
    pub numeric_consts: Vec<NumericConstant>,
}

/// This enumeration represents the two possible encodings of a bytecode
/// instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Instruction {
    ABC { a: u8, b: u8, c: u8, op: u8 },
    AD { a: u8, d: u16, op: u8 },
}

/// This enumeration represents an up-value constant in a LuaJIT bytecode
/// buffer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpValueConstant {
    /// Case where the up-value references a local slot in the parent
    /// prototype.
    ParentLocalSlot(u16),

    /// Case where the up-value references another up-value in the parent
    /// prototype.
    ParentUpValue(u16),
}

/// This enum represents a constant in a bytecode prototype.
#[derive(Debug, Clone, PartialEq)]
pub enum ComplexConstant {
    Child,
    Table {
        array_part: Vec<TableConstantElement>,
        hash_part: Vec<(TableConstantElement, TableConstantElement)>,
    },
    Integer(i64),
    UnsignedInteger(u64),
    String(String),
}

/// This enumeration represents an element of a table constant in a prototype
/// bytecode.
#[derive(Debug, Clone, PartialEq)]
pub enum TableConstantElement {
    Nil,
    False,
    True,
    Integer(i32),
    Float(f64),
    String(String),
}

/// This enumeration represents a numeric constant in a function prototype
/// bytecode.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum NumericConstant {
    Integer(i32),
    Float(f64),
}
