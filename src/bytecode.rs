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
//! ```text
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
//! ```text
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
//! stored in the local slot `0` and `b` in the slot `1`.
//!
//! ## The constant table
//!
//! `CONSTANT_TABLE` is a section at the end of every function prototype which
//! contains all constants used in the function along with some debug
//! information. Its structure is:
//!
//! ```text
//! UPVALUE_CONSTS (1 short16[UPVAL_COUNT])
//! COMPLEX_CONSTS (1 COMPLEX_CONST[COMPLEX_CONST_COUNT])
//! NUMERIC_CONSTS (1 NUMERIC_CONST[NUM_CONST_COUNT])
//! (
//!     DEBUG_INFO
//! ) if !IS_STRIPPED
//! ```
//!
//! ### Up-value constants
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
//! The complex constant table is reversed, meaning that constant with the
//! index 0 is the last one in the table.
//!
//! A `COMPLEX_CONST` in the LuaJIT bytecode is composed of two parts:
//!
//! ```text
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
//! ```text
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
//! ```text
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
//! ```text
//! LOW_PART = (1 uleb128)  // The 32 least significant bits of the f64 with
//!                         // the numeric constant flag on the first byte
//! HIGH_PART = (1 uleb128) // The 32 most significant bits of the f64
//! ```
//!
//! ### Debug information
//!
//! The `DEBUG_INFO` section is a byte array at the end of an constant table
//! that describes all required data to perform debugging operations on the
//! current prototype. It is split into 3 parts:
//!   * It starts with a sequence of line offsets, one for each instruction of
//!     the prototype. An offset value is representing the difference between
//!     the line the corresponding instruction is mapping to and the line the
//!     prototype is starting from.
//!     Each offset is encoded on the minimal unsigned number of bytes, meaning
//!     that according to the number of lines in the prototype:
//!       * <= 255: each line offset is encoded on 1 byte
//!       * > 255 & <= 65,535: each line offset is encoded on 2 bytes
//!       * > 65535 & <= 4,294,967,295: each line offset is encoded on 4 bytes
//!   * Then comes the up-value names section: this is a sequence of C-encoded
//!     strings representing names of the up-values used in this prototype.
//!     This sequence is in the same order as up-value constants.
//!   * Finally, there are information about the local variables of the
//!     prototype. This section is a sequence of variable data, ordered
//!     following variable declaration order.
//!     A variable data is structured like this:
//!       * The variable name in a C-encoded string (ASCII chars with `\0` at
//!         the end)
//!       * The variable birth instruction as an ULEB128 encoded instruction
//!         index difference from the previous local variable birth
//!         instruction. If the current variable is the first one in the
//!         sequence, this info is simply the birth instruction index.
//!       * The variable death instruction as an ULEB128 encoded instruction
//!         index difference from the birth instruction.
//!     The variable information section is always terminated by a `0` byte.

use crate::bytecode::op_codes::JMP;
use leb128;
use std::{fmt::Display, io::Write};

pub mod extended_bytecode;
pub mod op_codes;

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

// ----- Primitive values -----

pub const PRIM_NIL: u16 = 0;
pub const PRIM_FALSE: u16 = 1;
pub const PRIM_TRUE: u16 = 2;

// ----- Bytecode emission helpers -----

/// This structure represents an executable bytecode buffer, structured as
/// specified in the [`bytecode`](mod@self) module.
#[derive(Debug, PartialEq)]
pub struct BytecodeUnit {
    /// Name of the source this bytecode buffer is coming from.
    pub source_name: String,

    /// Function prototypes, the last one is considered as the main.
    pub prototypes: Vec<Prototype>,
}

impl Display for BytecodeUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, proto) in self.prototypes.iter().enumerate() {
            write!(f, "##### Prototype #{i} #####\n\n{}", proto)?;
            if i < self.prototypes.len() - 1 {
                write!(f, "\n\n")?;
            }
        }
        Ok(())
    }
}

impl BytecodeUnit {
    /// Encode the bytecode described by [`self`] in the provided output
    /// buffer.
    pub fn encode(&self, output_buffer: &mut Vec<u8>) {
        // Start by adding the magic bytes
        MAGIC.iter().for_each(|b| output_buffer.push(*b));

        // Add the bytecode version
        output_buffer.push(CUR_VERSION);

        // Add the flags byte
        output_buffer.push(
            if cfg!(target_endian = "big") { FLAG_H_IS_BIG_ENDIAN } else { 0 }
                | FLAG_H_HAS_FFI
                | FLAG_H_FR2,
        );

        // Add the source name
        write_uleb128(output_buffer, self.source_name.len() as u64);
        output_buffer.extend_from_slice(self.source_name.as_bytes());

        // Add prototypes
        self.prototypes.iter().for_each(|p| p.encode(output_buffer));

        // Add the final 0x0 byte
        output_buffer.push(0);
    }
}

/// This structure represents a function prototype in the LuaJIT bytecode
/// encoding.
#[derive(Debug, PartialEq)]
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

    // --- Up-values and constants
    pub up_values: Vec<UpValue>,
    pub complex_consts: Vec<ComplexConstant>,
    pub numeric_consts: Vec<NumericConstant>,

    // --- Debug information
    /// The index first line of the prototype, first line of the source being
    /// indexed as `1`.
    pub first_line: u32,

    /// Number of lines bellow the starting line that belongs to this
    /// prototype.
    pub line_count: u32,
    pub variable_data: Vec<VariableData>,
}

impl Display for Prototype {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn vec_image<T>(vec: &Vec<T>) -> String
        where
            T: Display,
        {
            if vec.is_empty() {
                String::from("[]")
            } else {
                let indent_str = "  ";
                format!(
                    "[\n{}\n]",
                    vec.iter()
                        .enumerate()
                        .map(|(i, v)| format!("{indent_str}{i} - {}", v))
                        .collect::<Vec<_>>()
                        .join("\n")
                )
            }
        }
        let instruction_padding = self.instructions.len().to_string().len();
        let format_inst = |(i, inst): (usize, &Instruction)| -> String {
            let additional_info = match inst.op_code {
                JMP => match &inst.variant {
                    InstructionVariant::AD { d, .. } => format!(
                        "  => {:0instruction_padding$}",
                        (*d as isize - JUMP_BIASING as isize + 1) + i as isize
                    ),
                    _ => unreachable!(),
                },
                _ => String::new(),
            };
            format!("{:0instruction_padding$}  {inst}{additional_info}", i)
        };
        write!(
            f,
            "--- Header\n\
            has_child:   {}\n\
            is_variadic: {}\n\
            has_ffi:     {}\n\
            arg_count:   {}\n\
            frame_size:  {}\n\n\
            --- Instructions\n\
            {}\n\n\
            --- Constants\n\
            up_values: {}\n\
            complexes: {}\n\
            numerics: {}",
            self.has_child,
            self.is_variadic,
            self.has_ffi,
            self.arg_count,
            self.frame_size,
            self.instructions
                .iter()
                .enumerate()
                .map(format_inst)
                .collect::<Vec<_>>()
                .join("\n"),
            vec_image(&self.up_values),
            vec_image(&self.complex_consts),
            vec_image(&self.numeric_consts),
        )
    }
}

impl Prototype {
    /// Encode the function prototype in the LuaJIT format in the given output
    /// buffer.
    pub fn encode(&self, output_buffer: &mut Vec<u8>) {
        // Create a new byte vector, containing all bytes of the prototype
        // except its size.
        let mut prototype_bytes = Vec::<u8>::new();

        // Add the flags byte
        prototype_bytes.push(
            if self.has_child { FLAG_P_HAS_CHILD } else { 0 }
                | if self.is_variadic { FLAG_P_IS_VARIADIC } else { 0 }
                | if self.has_ffi { FLAG_P_HAS_FFI } else { 0 },
        );

        // Add the argument count and the frame size
        prototype_bytes.push(self.arg_count);
        prototype_bytes.push(self.frame_size);

        // Ensure the number of up-values can be encoded in a byte
        if self.up_values.len() > u8::MAX as usize {
            panic!("Cannot encode the prototype {:?}, too much up-values", self);
        }

        // Add constant and up-value counts
        prototype_bytes.push(self.up_values.len() as u8);
        write_uleb128(&mut prototype_bytes, self.complex_consts.len() as u64);
        write_uleb128(&mut prototype_bytes, self.numeric_consts.len() as u64);

        // Add the instruction count
        write_uleb128(&mut prototype_bytes, self.instructions.len() as u64);

        // Get the debug section to know its size when emitting the first debug
        // info part.
        let mut debug_section = self.debug_section();

        // Write the first debug information part
        write_uleb128(&mut prototype_bytes, debug_section.len() as u64);
        write_uleb128(&mut prototype_bytes, self.first_line as u64);
        write_uleb128(&mut prototype_bytes, self.line_count as u64);

        // Add function instructions
        self.instructions
            .iter()
            .for_each(|i| i.encode(&mut prototype_bytes));

        // Add up-values and constants
        self.up_values
            .iter()
            .for_each(|uv| uv.encode(&mut prototype_bytes));
        self.complex_consts
            .iter()
            .rev()
            .for_each(|c| c.encode(&mut prototype_bytes));
        self.numeric_consts
            .iter()
            .for_each(|n| n.encode(&mut prototype_bytes));

        // Add the debug section
        prototype_bytes.append(&mut debug_section);

        // Finally copy the prototype bytes into the output buffer with its
        // size.
        write_uleb128(output_buffer, prototype_bytes.len() as u64);
        output_buffer.append(&mut prototype_bytes);
    }

    /// Util function to create the debug data section as a byte buffer and
    /// return it.
    fn debug_section(&self) -> Vec<u8> {
        // Create the debug data section
        let mut res: Vec<u8> = Vec::new();

        // Add line offsets
        let mut offset_buffer: Vec<u8> = Vec::with_capacity(4);
        for i in &self.instructions {
            let offset = i.source_line - self.first_line;
            match self.line_count {
                0..256 => offset_buffer.push(offset as u8),
                256..65536 => offset_buffer.extend_from_slice(&(offset as u16).to_ne_bytes()),
                65536.. => offset_buffer.extend_from_slice(&(offset as u32).to_ne_bytes()),
            };
            res.append(&mut offset_buffer);
        }

        // Append up-value names
        for uv in &self.up_values {
            res.extend_from_slice(uv.name.as_bytes());
            res.push(0);
        }

        // Then, add all variable information
        let mut previous_data: Option<&VariableData> = None;
        for data in &self.variable_data {
            data.encode(&mut res, previous_data);
            let _ = previous_data.insert(data);
        }

        // Finally place the `0` at the debug section end and return the
        // result.
        res.push(0);
        res
    }
}

/// This type represents an instruction in a bytecode prototype.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Instruction {
    /// Operation code of this instruction.
    pub op_code: u8,

    /// Variant part of the instruction, whether it as the AD or the ABC
    /// layout.
    pub variant: InstructionVariant,

    /// The source line this instruction is from.
    pub source_line: u32,
}

/// This enum represents the variant part of an instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InstructionVariant {
    ABC { a: u8, b: u8, c: u8 },
    AD { a: u8, d: u16 },
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.variant {
            InstructionVariant::ABC { a, b, c } => write!(
                f,
                "{: <6} | {: <3} | {: <3} | {: <3}",
                op_codes::NAME_ARRAY[self.op_code as usize],
                a,
                b,
                c,
            ),
            InstructionVariant::AD { a, d } => {
                if self.op_code == JMP {
                    write!(f, "JMP    | {: <3} | {:#06x}", a, d)
                } else {
                    write!(
                        f,
                        "{: <6} | {: <3} | {: <6}",
                        op_codes::NAME_ARRAY[self.op_code as usize],
                        a,
                        d,
                    )
                }
            }
        }
    }
}

impl Instruction {
    /// Shortcut function to create an instruction with the `ABC` variant.
    pub fn abc(op_code: u8, a: u8, b: u8, c: u8, source_line: u32) -> Self {
        Self { op_code, variant: InstructionVariant::ABC { a, b, c }, source_line }
    }

    /// Shortcut function to create an instruction with the `AD` variant.
    pub fn ad(op_code: u8, a: u8, d: u16, source_line: u32) -> Self {
        Self { op_code, variant: InstructionVariant::AD { a, d }, source_line }
    }

    /// Encode the instruction as a byte vector, following the current system
    /// byte order.
    pub fn encode(&self, output_buffer: &mut Vec<u8>) {
        let mut le_inst = match &self.variant {
            InstructionVariant::ABC { a, b, c } => vec![self.op_code, *a, *c, *b],
            InstructionVariant::AD { a, d } => {
                let mut res = vec![self.op_code, *a];
                res.append(&mut d.to_le_bytes().to_vec());
                res
            }
        };
        if cfg!(target_endian = "big") {
            le_inst.reverse();
        }
        le_inst.iter().for_each(|b| output_buffer.push(*b));
    }

    pub fn jump_forward(offset: u16) -> u16 {
        JUMP_BIASING + offset - 1
    }

    pub fn jump_backward(offset: u16) -> u16 {
        JUMP_BIASING - offset - 1
    }
}

/// This type represents an up-value in a bytecode prototype.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UpValue {
    /// Name of the up-value.
    pub name: String,

    /// The variant part of the up-value.
    pub variant: UpValueVariant,
}

/// This enumeration represents the variant part of an up-value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UpValueVariant {
    /// Case where the up-value references a local slot in the parent
    /// prototype.
    ParentLocalSlot(u16),

    /// Case where the up-value references another up-value in the parent
    /// prototype.
    ParentUpValue(u16),
}

impl Display for UpValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl UpValue {
    /// Shortcut function to create an up-value to a local slot of the parent
    /// frame.
    pub fn parent_local(name: &str, local_slot: u16) -> Self {
        UpValue {
            name: String::from(name),
            variant: UpValueVariant::ParentLocalSlot(local_slot),
        }
    }

    /// Shortcut function to create an up-value to an up-value of the parent
    /// frame.
    pub fn parent_up_value(name: &str, up_value: u16) -> Self {
        UpValue { name: String::from(name), variant: UpValueVariant::ParentUpValue(up_value) }
    }

    /// Encode the up-value constant to the LuaJIT bytecode format and add the
    /// result to the provided output buffer.
    pub fn encode(&self, output_buffer: &mut Vec<u8>) {
        match &self.variant {
            UpValueVariant::ParentLocalSlot(s) => {
                let final_val = s | 0xC000;
                output_buffer.write_all(&final_val.to_ne_bytes()).unwrap();
            }
            UpValueVariant::ParentUpValue(s) => {
                output_buffer.write_all(&s.to_ne_bytes()).unwrap();
            }
        }
    }
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

impl Display for ComplexConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl ComplexConstant {
    /// Encode the complex constant in the LuaJIT bytecode format and add the
    /// result to the provided output buffer.
    pub fn encode(&self, output_buffer: &mut Vec<u8>) {
        match self {
            ComplexConstant::Child => output_buffer.push(COMPLEX_CONST_CHILD_KIND),
            ComplexConstant::Table { array_part, hash_part } => {
                output_buffer.push(COMPLEX_CONST_TAB_KIND);
                let array_size = if array_part.is_empty() { 0 } else { array_part.len() + 1 };
                write_uleb128(output_buffer, array_size as u64);
                write_uleb128(output_buffer, hash_part.len() as u64);
                if !array_part.is_empty() {
                    output_buffer.push(TABLE_CONST_NIL_KIND);
                }
                array_part.iter().for_each(|c| c.encode(output_buffer));
                hash_part.iter().for_each(|(k, v)| {
                    k.encode(output_buffer);
                    v.encode(output_buffer);
                });
            }
            ComplexConstant::Integer(i) => {
                output_buffer.push(COMPLEX_CONST_I64_KIND);
                write_uleb128(output_buffer, *i as u64);
            }
            ComplexConstant::UnsignedInteger(u) => {
                output_buffer.push(COMPLEX_CONST_U64_KIND);
                write_uleb128(output_buffer, *u);
            }
            ComplexConstant::String(s) => encode_string_constant(s, output_buffer),
        }
    }
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

impl Display for TableConstantElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl TableConstantElement {
    /// Encode the table constant in the LuaJIT bytecode format and add the
    /// result to the provided output buffer.
    pub fn encode(&self, output_buffer: &mut Vec<u8>) {
        match self {
            TableConstantElement::Nil => output_buffer.push(TABLE_CONST_NIL_KIND),
            TableConstantElement::False => output_buffer.push(TABLE_CONST_FALSE_KIND),
            TableConstantElement::True => output_buffer.push(TABLE_CONST_TRUE_KIND),
            TableConstantElement::Integer(i) => {
                output_buffer.push(TABLE_CONST_INT_KIND);
                write_uleb128(output_buffer, (*i as u32) as u64);
            }
            TableConstantElement::Float(f) => {
                let float_bits = f.to_bits();
                output_buffer.push(TABLE_CONST_NUM_KIND);
                write_uleb128(output_buffer, float_bits & 0xFFFFFFFF);
                write_uleb128(output_buffer, float_bits >> 32);
            }
            TableConstantElement::String(s) => encode_string_constant(s, output_buffer),
        }
    }
}

/// This enumeration represents a numeric constant in a function prototype
/// bytecode.
#[derive(Debug, Clone, PartialEq)]
pub enum NumericConstant {
    Integer(i32),
    Float(f64),
}

impl Display for NumericConstant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl NumericConstant {
    /// Encode the numeric constant in the LuaJIT bytecode format and add the
    /// result in the provided output buffer.
    pub fn encode(&self, output_buffer: &mut Vec<u8>) {
        match self {
            NumericConstant::Integer(i) => write_uleb128(output_buffer, ((*i as u32) as u64) << 1),
            NumericConstant::Float(f) => {
                let float_bits = f.to_bits();
                write_uleb128(output_buffer, ((float_bits & 0xFFFFFFFF) << 1) | 1);
                write_uleb128(output_buffer, float_bits >> 32);
            }
        }
    }
}

/// This type represents debug information for a local variable.
#[derive(Debug, Clone, PartialEq)]
pub struct VariableData {
    pub name: String,

    /// Instruction index (0-based) from which this variable becomes available
    /// to read.
    pub birth_instruction: usize,

    /// Instruction index (0-based) from which this variable is not readable
    /// anymore.
    pub death_instruction: usize,
}

impl VariableData {
    /// Encode the variable data in the LuaJIT bytecode format and add the
    /// result to the provided output buffer.
    pub fn encode(
        &self,
        output_buffer: &mut Vec<u8>,
        previous_variable_data: Option<&VariableData>,
    ) {
        // Push the `\0` terminated variable name
        output_buffer.extend_from_slice(self.name.as_bytes());
        output_buffer.push(0);

        // Compute and write the compressed life range of the variable
        write_uleb128(
            output_buffer,
            (self.birth_instruction + 1
                - previous_variable_data.map_or(0, |v| v.birth_instruction + 1)) as u64,
        );
        write_uleb128(output_buffer, (self.death_instruction - self.birth_instruction) as u64);
    }
}

/// Function to write an ULEB128 encoded value into the given output buffer.
fn write_uleb128(output_buffer: &mut Vec<u8>, val: u64) {
    leb128::write::unsigned(output_buffer, val).unwrap();
}

/// Function to encode a string into the LuaJIT constant format and append the
/// result to the provided output buffer.
fn encode_string_constant(s: &String, output_buffer: &mut Vec<u8>) {
    write_uleb128(output_buffer, (s.len() + COMPLEX_CONST_STR_KIND as usize) as u64);
    for b in s.bytes() {
        output_buffer.push(b);
    }
}

mod tests {
    #[allow(unused)]
    use super::*;

    #[test]
    fn test_prototype_encode() {
        let mut bytes = Vec::<u8>::new();

        Prototype {
            has_child: true,
            is_variadic: false,
            has_ffi: true,
            arg_count: 3,
            frame_size: 4,
            instructions: vec![Instruction::ad(3, 1, 2, 1), Instruction::abc(4, 1, 2, 3, 2)],
            up_values: vec![
                UpValue::parent_local("uv1", 0),
                UpValue::parent_up_value("uv2", 1),
            ],
            complex_consts: vec![ComplexConstant::Child],
            numeric_consts: vec![NumericConstant::Integer(42)],
            first_line: 0,
            line_count: 2,
            variable_data: vec![
                VariableData {
                    name: String::from("var1"),
                    birth_instruction: 1,
                    death_instruction: 3,
                },
                VariableData {
                    name: String::from("var2"),
                    birth_instruction: 2,
                    death_instruction: 3,
                },
            ],
        }
        .encode(&mut bytes);
        assert_eq!(
            bytes,
            vec![
                0x31, // Prototype size
                0x05, // Flags
                0x03, // Arg count
                0x04, // Frame size
                0x02, // Up-value count
                0x01, // Complex constant count
                0x01, // Numeric constant count
                0x02, // Instruction count
                0x19, // Debug info size
                0x00, // First line
                0x02, // Line count
                0x03, 0x01, 0x02, 0x00, // Instruction #1
                0x04, 0x01, 0x03, 0x02, // Instruction #2
                0x00, 0xC0, // Up-value #1
                0x01, 0x00, // Up-value #2
                0x00, // Child constant
                0x54, // Integer constant
                0x01, 0x02, // Line offsets
                'u' as u8, 'v' as u8, '1' as u8, 0x00, // Up-value #1 name
                'u' as u8, 'v' as u8, '2' as u8, 0x00, // Up-value #2 name
                'v' as u8, 'a' as u8, 'r' as u8, '1' as u8, 0x00, // Variable #1 name
                0x02, 0x02, // Variable #1 lifespan
                'v' as u8, 'a' as u8, 'r' as u8, '2' as u8, 0x00, // Variable #2 name
                0x01, 0x01, // Variable #1 lifespan
                0x00, // Debug info end
            ]
        )
    }

    #[test]
    fn test_up_value_constant_encode() {
        let mut bytes = Vec::<u8>::new();

        // Try encoding a parent slot referencing up-value
        UpValue::parent_local("dum", 42).encode(&mut bytes);
        assert_eq!(bytes, vec![0x2A, 0xC0]);
        bytes.clear();

        // Try encoding a parent up-value referencing up-value
        UpValue::parent_up_value("dum", 42).encode(&mut bytes);
        assert_eq!(bytes, vec![0x2A, 0x00]);
        bytes.clear();
    }

    #[test]
    fn test_complex_constant_encode() {
        let mut bytes = Vec::<u8>::new();

        // Try encoding a child constant
        ComplexConstant::Child.encode(&mut bytes);
        assert_eq!(bytes, vec![0x0]);
        bytes.clear();

        // Try encoding a table constant
        let one_constant = TableConstantElement::Integer(1);
        ComplexConstant::Table {
            array_part: vec![one_constant.clone()],
            hash_part: vec![(TableConstantElement::String(String::from("a")), one_constant)],
        }
        .encode(&mut bytes);
        assert_eq!(bytes, vec![0x01, 0x02, 0x01, 0x00, 0x03, 0x01, 0x06, 0x61, 0x03, 0x01]);
        bytes.clear();

        // Try encoding a positive integer constant
        ComplexConstant::Integer(42).encode(&mut bytes);
        assert_eq!(bytes, vec![0x02, 0x2A]);
        bytes.clear();

        // Try encoding a negative integer constant
        ComplexConstant::Integer(-42).encode(&mut bytes);
        assert_eq!(
            bytes,
            vec![
                0x02, 0xD6, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01
            ]
        );
        bytes.clear();

        // Try encoding a unsigned integer constant
        ComplexConstant::UnsignedInteger(u64::MAX).encode(&mut bytes);
        assert_eq!(
            bytes,
            vec![
                0x03, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x01
            ]
        );
        bytes.clear();

        // Try encoding a string constant
        ComplexConstant::String(String::from("Hello!")).encode(&mut bytes);
        assert_eq!(bytes, vec![0x0B, 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x21]);
        bytes.clear();
    }

    #[test]
    fn test_table_constant_element_encode() {
        let mut bytes = Vec::<u8>::new();

        // Try encoding a nil constant
        TableConstantElement::Nil.encode(&mut bytes);
        assert_eq!(bytes, vec![0x00]);
        bytes.clear();

        // Try encoding a false constant
        TableConstantElement::False.encode(&mut bytes);
        assert_eq!(bytes, vec![0x01]);
        bytes.clear();

        // Try encoding a true constant
        TableConstantElement::True.encode(&mut bytes);
        assert_eq!(bytes, vec![0x02]);
        bytes.clear();

        // Try encoding a positive integer constant
        TableConstantElement::Integer(42).encode(&mut bytes);
        assert_eq!(bytes, vec![0x03, 0x2A]);
        bytes.clear();

        // Try encoding a negative integer constant
        TableConstantElement::Integer(-42).encode(&mut bytes);
        assert_eq!(bytes, vec![0x03, 0xD6, 0xFF, 0xFF, 0xFF, 0x0F]);
        bytes.clear();

        // Try encoding an float constant
        TableConstantElement::Float(1.1).encode(&mut bytes);
        assert_eq!(
            bytes,
            vec![
                0x04, 0x9A, 0xB3, 0xE6, 0xCC, 0x09, 0x99, 0xB3, 0xC6, 0xFF, 0x03
            ]
        );
        bytes.clear();

        // Try encoding an string constant
        TableConstantElement::String(String::from("Hello!")).encode(&mut bytes);
        assert_eq!(bytes, vec![0x0B, 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x21]);
        bytes.clear();
    }

    #[test]
    fn test_numeric_constant_encode() {
        let mut bytes = Vec::<u8>::new();

        // Test encoding a positive integer constant
        NumericConstant::Integer(4625538).encode(&mut bytes);
        assert_eq!(bytes, vec![0x84, 0xD2, 0xB4, 0x04]);
        bytes.clear();

        // Test encoding a negative integer constant
        NumericConstant::Integer(-4625538).encode(&mut bytes);
        assert_eq!(bytes, vec![0xFC, 0xAD, 0xCB, 0xFB, 0x1F]);
        bytes.clear();

        // Test encoding a float constant
        NumericConstant::Float(1.1).encode(&mut bytes);
        assert_eq!(bytes, vec![0xB5, 0xE6, 0xCC, 0x99, 0x13, 0x99, 0xB3, 0xC6, 0xFF, 0x03]);
        bytes.clear();
    }

    #[test]
    fn test_write_uleb128() {
        let mut bytes = Vec::<u8>::new();

        // Test encoding a small number in uleb128
        write_uleb128(&mut bytes, 42);
        assert_eq!(bytes, vec![0x2A]);
        bytes.clear();

        // Test encoding a big number in uleb128
        write_uleb128(&mut bytes, 4625538);
        assert_eq!(bytes, vec![0x82, 0xA9, 0x9A, 0x02]);
        bytes.clear();
    }

    #[test]
    fn test_encode_string_constant() {
        let mut bytes = Vec::<u8>::new();

        // Test encoding a simple string
        encode_string_constant(&String::from("Hello!"), &mut bytes);
        assert_eq!(bytes, vec![0x0B, 0x48, 0x65, 0x6C, 0x6C, 0x6F, 0x21]);
        bytes.clear();

        // Test encoding an empty string
        encode_string_constant(&String::from(""), &mut bytes);
        assert_eq!(bytes, vec![0x05]);
        bytes.clear();

        // Test encoding an UTF-8 string
        encode_string_constant(&String::from("Ϣ"), &mut bytes);
        assert_eq!(bytes, vec![0x07, 0xCF, 0xA2]);
        bytes.clear();
    }
}
