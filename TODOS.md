# TODOS

## Idea dump

## Features

* Handle list comprehensions
* Compile and execute all the general purpose subset
* Create a way to access value types at runtime when reporting errors
* Report "too many arguments" errors (?)

## Cleanups

* Move the `metatable_global_field` in the `BuiltinType` type
* Remove "bug_msg" and other things like this to replace them by `panic!` and `assert!`. We don't want to fail gracefully in some
  cases, and `Report` should be reserved to compilation and runtime error, not exception / bug in the compiler

## Performances

### Emit optimized array access instructions

When accessing a list index with an integer literal there is no need to check its type and you can possibly optimize
the instruction by emitting a `TGETB`

### Check what the "ISTYPE" operation is doing

For now runtime type checking is only based on custom type tags which required to access a value field. This is very expensive
regarding performances. Maybe I can rely on the "ISTYPE" instruction to perform type checking for data types (int, string and boolean)
