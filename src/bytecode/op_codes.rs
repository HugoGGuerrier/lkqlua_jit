#![allow(unused)]

//! # Bytecode instructions
//!
//! This module contains all available LuaJIT bytecode operations codes, with
//! a comprehensive documentation about their format and semantics.
//! In the LuaJIT bytecode format, an instruction is always the same size:
//! 32bits. An instruction structure may be one of the two following:
//!
//! ```text
//! B (1 byte)
//! C (1 byte)
//! A (1 byte)
//! OP (1 byte)
//! ```
//!
//! Or
//!
//! ```text
//! D (2 bytes)
//! A (1 byte)
//! OP (1 byte)
//! ```
//!
//! With:
//!   * `A`, `B`, `C` and `D` -> Operands
//!   * `OP` -> Operation code
//!
//! The kind of an instruction is defined by its operation code.
//! Instructions operands may have on of the following types:
//!   * slot -> Local variable slot index
//!   * rslot -> Local variable slot index (read-only)
//!   * uv: Up-value index
//!   * lit: Literal
//!   * slit: Signed literal
//!   * pri: Primitive value (0 = nil, 1 = false, 2 = true)
//!   * num: Number constant, index into constant table
//!   * string: String constant, index into constant table
//!   * tab: Template table, index into constant table
//!   * func: Function prototype, index into constant table
//!   * cdata: Complex constant, index into constant table
//!   * jump: Branch target, relative to next instruction, biased with 0x8000
//!
//! Additionally to operands, the `MULTRES` variable may be accessed by
//! some operations to perform computing. `MULTRES` is an internal variable
//! that keeps track of the number of results returned by the previous call or
//! by `VARG` operation.

// ----- Variable comparison operations -----

/// Jump if A < D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: slot
/// ```
pub const ISLT: u8 = 0x00;

/// Jump if A >= D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: slot
/// ```
pub const ISGE: u8 = 0x01;

/// Jump if A <= D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: slot
/// ```
pub const ISLE: u8 = 0x02;

/// Jump if A > D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: slot
/// ```
pub const ISGT: u8 = 0x03;

/// Jump if A == D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: slot
/// ```
pub const ISEQV: u8 = 0x04;

/// Jump if A != D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: slot
/// ```
pub const ISNEV: u8 = 0x05;

// ----- String constant comparison operations -----

/// Jump if A == D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: string
/// ```
pub const ISEQS: u8 = 0x06;

/// Jump if A != D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: string
/// ```
pub const ISNES: u8 = 0x07;

// ----- Numeric constant comparison operations -----

/// Jump if A == D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: num
/// ```
pub const ISEQN: u8 = 0x08;

/// Jump if A != D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: num
/// ```
pub const ISNEN: u8 = 0x09;

// ----- Primitive comparison operations -----

/// Jump if A == D (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// A: slot
/// D: pri
/// ```
pub const ISEQP: u8 = 0x0A;

/// Jump if A != D (Always followed by a [`JMP`] instruction holding the
/// branching target)/
/// ```text
/// A: slot
/// D: pri
/// ```
pub const ISNEP: u8 = 0x0B;

// ----- Unary copy and test operations -----

/// Copy D to A and jump, if D is true (Always followed by a [`JMP`]
/// instruction holding the branching target).
/// ```text
/// A: slot
/// D: slot
/// ```
pub const ISTC: u8 = 0x0C;

/// Copy D to A and jump, if D is false (Always followed by a [`JMP`]
/// instruction holding the branching target).
/// ```text
/// A: slot
/// D: slot
/// ```
pub const ISFC: u8 = 0x0D;

// ----- Unary test operations -----

/// Jump if D is true (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// D: slot
/// ```
pub const IST: u8 = 0x0E;

/// Jump if D is false (Always followed by a [`JMP`] instruction holding the
/// branching target).
/// ```text
/// D: slot
/// ```
pub const ISF: u8 = 0x0F;

/// TODO
pub const ISTYPE: u8 = 0x10;

/// TODO
pub const ISNUM: u8 = 0x11;

// ----- Copying operations -----

/// Copy D to A.
/// ```text
/// A: slot
/// D: slot
/// ```
pub const MOV: u8 = 0x12;

// ----- Unary operations -----

/// Set A to !D.
/// ```text
/// A: slot
/// D: slot
/// ```
pub const NOT: u8 = 0x13;

/// Set A to -D.
/// ```text
/// A: slot
/// D: slot
/// ```
pub const UNM: u8 = 0x14;

/// Set A to #D (length of D).
/// ```text
/// A: slot
/// D: slot
/// ```
pub const LEN: u8 = 0x15;

// ----- Numeric constant binary operations -----

/// Set A to B + C.
/// ```text
/// A: slot
/// B: slot
/// C: num
/// ```
pub const ADDVN: u8 = 0x16;

/// Set A to B - C.
/// ```text
/// A: slot
/// B: slot
/// C: num
/// ```
pub const SUBVN: u8 = 0x17;

/// Set A to B * C.
/// ```text
/// A: slot
/// B: slot
/// C: num
/// ```
pub const MULVN: u8 = 0x18;

/// Set A to B / C.
/// ```text
/// A: slot
/// B: slot
/// C: num
/// ```
pub const DIVVN: u8 = 0x19;

/// Set A to B % C.
/// ```text
/// A: slot
/// B: slot
/// C: num
/// ```
pub const MODVN: u8 = 0x1A;

// ----- Reversed numeric constant binary operations -----

/// Set A to C + B.
/// ```text
/// A: slot
/// B: slot
/// C: num
/// ```
pub const ADDNV: u8 = 0x1B;

/// Set A to C - B.
/// ```text
/// A: slot
/// B: slot
/// C: num
/// ```
pub const SUBNV: u8 = 0x1C;

/// Set A to C * B.
/// ```text
/// A: slot
/// B: slot
/// C: num
/// ```
pub const MULNV: u8 = 0x1D;

/// Set A to C / B.
/// ```text
/// A: slot
/// B: slot
/// C: num
/// ```
pub const DIVNV: u8 = 0x1E;

/// Set A to C % B.
/// ```text
/// A: slot
/// B: slot
/// C: num
/// ```
pub const MODNV: u8 = 0x1F;

// ----- Variable binary operations -----

/// Set A to B + C.
/// ```text
/// A: slot
/// B: slot
/// C: slot
/// ```
pub const ADDVV: u8 = 0x20;

/// Set A to B - C.
/// ```text
/// A: slot
/// B: slot
/// C: slot
/// ```
pub const SUBVV: u8 = 0x21;

/// Set A to B * C.
/// ```text
/// A: slot
/// B: slot
/// C: slot
/// ```
pub const MULVV: u8 = 0x22;

/// Set A to B / C.
/// ```text
/// A: slot
/// B: slot
/// C: slot
/// ```
pub const DIVVV: u8 = 0x23;

/// Set A to B % C.
/// ```text
/// A: slot
/// B: slot
/// C: slot
/// ```
pub const MODVV: u8 = 0x24;

/// Set A to B ^ C (B power C).
/// ```text
/// A: slot
/// B: slot
/// C: slot
/// ```
pub const POW: u8 = 0x25;

/// Set A to the result of the concatenation of all values from B to C (both
/// inclusive).
/// ```text
/// A: slot
/// B: slot
/// C: slot
/// ```
pub const CAT: u8 = 0x26;

// ----- Constant loading operations -----

/// Set A to string constant D.
/// ```text
/// A: slot
/// D: string
/// ```
pub const KSTR: u8 = 0x27;

/// Set A to complex constant D.
/// ```text
/// A: slot
/// D: cdata
/// ```
pub const KCDATA: u8 = 0x28;

/// Set A to signed short D.
/// ```text
/// A: slot
/// D: slit
/// ```
pub const KSHORT: u8 = 0x29;

/// Set A to numeric constant D.
/// ```text
/// A: slot
/// D: num
/// ```
pub const KNUM: u8 = 0x2A;

/// Set A to primitive value D.
/// ```text
/// A: slot
/// D: pri
/// ```
pub const KPRI: u8 = 0x2B;

/// Set slots from A to D to nil.
/// ```text
/// A: slot
/// D: slot
/// ```
pub const KNIL: u8 = 0x2C;

// ----- Up-value operations -----

/// Set A to up-value D.
/// ```text
/// A: slot
/// D: uv
/// ```
pub const UGET: u8 = 0x2D;

/// Set up-value A to D.
/// ```text
/// A: uv
/// D: slot
/// ```
pub const USETV: u8 = 0x2E;

/// Set up-value A to string constant D.
/// ```text
/// A: uv
/// D: string
/// ```
pub const USETS: u8 = 0x2F;

/// Set up-value A to numeric constant D.
/// ```text
/// A: uv
/// D: num
/// ```
pub const USETN: u8 = 0x30;

/// Set up-value A to primitive value D.
/// ```text
/// A: uv
/// D: pri
/// ```
pub const USETP: u8 = 0x31;

/// Close up-values for slots >= A and jump to D.
/// ```text
/// A: rslot
/// D: jump
/// ```
pub const UCLO: u8 = 0x32;

// ----- Function operations -----

/// Create new closure from prototype D and store it in A.
/// ```text
/// A: slot
/// D: func
/// ```
pub const FNEW: u8 = 0x33;

// ----- Table creation operations -----

/// Set A to new table with size D.
///
/// D is split into two fields:
///   * 11 lowest bits give the array part size as unsigned int
///   * 5 highest bits give the hash part size as a power of 2
///
/// ```text
/// A: slot
/// D: lit
/// ```
pub const TNEW: u8 = 0x34;

/// Duplicate table D and set the result to A.
/// ```text
/// A: slot
/// D: tab
/// ```
pub const TDUP: u8 = 0x35;

// ----- Global table operations -----

/// Set A to global_table[D].
/// ```text
/// A: slot
/// D: string
/// ```
pub const GGET: u8 = 0x36;

/// Set global_table[D] to A.
/// ```text
/// A: slot
/// D: string
/// ```
pub const GSET: u8 = 0x37;

// ----- Table fetching operations -----

/// Set A to B[C].
/// ```text
/// A: slot
/// B: slot
/// C: slot
/// ```
pub const TGETV: u8 = 0x38;

/// Set A to B[C].
/// ```text
/// A: slot
/// B: slot
/// C: string
/// ```
pub const TGETS: u8 = 0x39;

/// Set A to B[C].
/// ```text
/// A: slot
/// B: slot
/// C: lit
/// ```
pub const TGETB: u8 = 0x3A;

/// TODO
pub const TGETR: u8 = 0x3B;

// ----- Table setting operations -----

/// Set B[C] to A.
/// ```text
/// A: slot
/// B: slot
/// C: slot
/// ```
pub const TSETV: u8 = 0x3C;

/// Set B[C] to A.
/// ```text
/// A: slot
/// B: slot
/// C: string
/// ```
pub const TSETS: u8 = 0x3D;

/// Set B[C] to A.
/// ```text
/// A: slot
/// B: slot
/// C: lit
/// ```
pub const TSETB: u8 = 0x3E;

/// Set A-1[D],A-1[D+1]...A-1[D+MULTRES] = A,A+1...A+MULTRES.
/// D must be a floating point constant and only the 32 lowest bits of the
/// mantissa is going to be used as an index.
/// ```text
/// A: slot
/// D: num
/// ```
pub const TSETM: u8 = 0x3F;

/// TODO
pub const TSETR: u8 = 0x40;

// ----- Function calling operations -----

/// Call A(A+2...A+C+MULTRES) and set A,A+1...A+B-2 to returned values.
/// ```text
/// A: slot
/// B: lit
/// C: lit
/// ```
pub const CALLM: u8 = 0x41;

/// Call A(A+2...A+C) and set A,A+1...A+B-2 to returned values.
/// ```text
/// A: slot
/// B: lit
/// C: lit
/// ```
pub const CALL: u8 = 0x42;

/// Call A(A+2...A+D+MULTRES) and immediately return resulting values.
/// ```text
/// A: slot
/// D: lit
/// ```
pub const CALLMT: u8 = 0x43;

/// Call A(A+2...A+D-1) and immediately return resulting values.
/// ```text
/// A: slot
/// D: lit
/// ```
pub const CALLT: u8 = 0x44;

// ----- Iterator and var-args handling operations -----

/// Call an iterator function located at A-3 with parameters A-2 and A-1,
/// placing the result into A,A+1...A+B-2.
/// This instruction perform a copy of A-3, A-2 and A-1 respectively into slots
/// A, A+1 and A+2 to preserve iteration information.
/// C is always 3 because an iteration function always accepts 2 parameters.
/// ```text
/// A: slot
/// B: lit
/// C: lit
/// ```
pub const ITERC: u8 = 0x45;

/// Specialized version of the [`ITERC`] instruction, if the iteration function
/// is `next`.
pub const ITERN: u8 = 0x46;

/// Set A,A+1...A+B-2 to variable arguments. C holds the number of fixed
/// arguments of the enclosing function.
/// ```text
/// A: slot
/// B: lit
/// C: lit
/// ```
pub const VARG: u8 = 0x47;

/// Runtime check to ensure the iteration function is `next`, if not, the loop
/// is un-specialized into generic [`ITERC`] and [`JMP`].
pub const ISNEXT: u8 = 0x48;

// ----- Return operations -----

/// Return A+A+1...A+D+MULTRES-1.
/// ```text
/// A: slot
/// D: lit
/// ```
pub const RETM: u8 = 0x49;

/// Return A+A+1...A+D-2.
/// ```text
/// A: rslot
/// D: lit
/// ```
pub const RET: u8 = 0x4A;

/// Return nothing.
/// ```text
/// A: rslot
/// D: lit
/// ```
pub const RET0: u8 = 0x4B;

/// Return A.
/// ```text
/// A: rslot
/// D: lit
/// ```
pub const RET1: u8 = 0x4C;

// ----- Numeric loop operations -----

/// Numeric loop initialization, A is the base slot from which loop information
/// are contained:
///   * A -> The loop init index
///   * A+1 -> The loop stop bound
///   * A+2 -> The loop step
///   * A+3 -> Is the loop current index, this slot is modified when calling
///     a [`FORL`] instruction.
/// D is just pointing to the instruction located immediately after the loop.
/// ```text
/// A: slot
/// D: jump
/// ```
pub const FORI: u8 = 0x4D;

/// JIT-compiled numeric loop initialization (do not use in code generation)
pub const JFORI: u8 = 0x4E;

/// Step forward in the loop execution:
///   * Set A to A + A+2
///   * Test if the loop is finished, if so just continue to the next
///     instruction, else set A+3 to A and jump to D
/// ```text
/// A: slot
/// D: jump
/// ```
pub const FORL: u8 = 0x4F;

/// Interpreted numeric loop step forward (do not use in code generation).
pub const IFORL: u8 = 0x50;

/// JIT-compiled numeric loop step forward (do not use in code generation).
pub const JFORL: u8 = 0x51;

// ----- Iterator loop operations -----

/// Check that the first result of an [`ITERC`] instruction is not `nil`, if so
/// the result is copied to A-1 and jump to D, else just continue to the next
/// instruction.
pub const ITERL: u8 = 0x52;

/// Interpreted iterator loop test (do not use in code generation).
pub const IITERL: u8 = 0x53;

/// JIT-compiled iterator loop test (do not use in code generation).
pub const JITERL: u8 = 0x54;

// ----- Generic loop operations -----

/// Generic loop instruction, used by the engine for tracing purposes.
pub const LOOP: u8 = 0x55;

/// Generic loop instruction, used by the engine for tracing purposes.
pub const ILOOP: u8 = 0x56;

/// Generic loop instruction, used by the engine for tracing purposes.
pub const JLOOP: u8 = 0x57;

// ----- Simple branching -----

/// Unconditionally branch to D, A is the next available slot at this stage.
/// ```text
/// A: rslot
/// D: jump
/// ```
pub const JMP: u8 = 0x58;

// ----- Function headers -----

/// Declare a function with fixed arguments, used by the engine for tracing
/// purposes.
pub const FUNCF: u8 = 0x59;

/// Declare a function with fixed arguments, force interpreter.
pub const IFUNCF: u8 = 0x5A;

/// Declare a function with fixed arguments, JIT-compiled.
pub const JFUNCF: u8 = 0x5B;

/// Declare a function with var-args, used by the engine for tracing purposes.
pub const FUNCV: u8 = 0x5C;

/// Declare a function with var-args, force interpreter.
pub const IFUNCV: u8 = 0x5D;

/// Declare a function with var-args, JIT-compiled.
pub const JFUNCV: u8 = 0x5E;

/// Declare a C function, used by the engine for tracing purposes.
pub const FUNCC: u8 = 0x5F;

/// Declare a C wrapped function, used by the engine for tracing purposes.
pub const FUNCCW: u8 = 0x60;

// ----- Name array -----

/// This array associate each code operation to its name
pub const NAME_ARRAY: [&str; 97] = [
    "ISLT", "ISGE", "ISLE", "ISGT", "ISEQV", "ISNEV", "ISEQS", "ISNES", "ISEQN", "ISNEN", "ISEQP",
    "ISNEP", "ISTC", "ISFC", "IST", "ISF", "ISTYPE", "ISNUM", "MOV", "NOT", "UNM", "LEN", "ADDVN",
    "SUBVN", "MULVN", "DIVVN", "MODVN", "ADDNV", "SUBNV", "MULNV", "DIVNV", "MODNV", "ADDVV",
    "SUBVV", "MULVV", "DIVVV", "MODVV", "POW", "CAT", "KSTR", "KCDATA", "KSHORT", "KNUM", "KPRI",
    "KNIL", "UGET", "USETV", "USETS", "USETN", "USETP", "UCLO", "FNEW", "TNEW", "TDUP", "GGET",
    "GSET", "TGETV", "TGETS", "TGETB", "TGETR", "TSETV", "TSETS", "TSETB", "TSETM", "TSETR",
    "CALLM", "CALL", "CALLMT", "CALLT", "ITERC", "ITERN", "VARG", "ISNEXT", "RETM", "RET", "RET0",
    "RET1", "FORI", "JFORI", "FORL", "IFORL", "JFORL", "ITERL", "IITERL", "JITERL", "LOOP",
    "ILOOP", "JLOOP", "JMP", "FUNCF", "IFUNCF", "JFUNCF", "FUNCV", "IFUNCV", "JFUNCV", "FUNCC",
    "FUNCCW",
];
