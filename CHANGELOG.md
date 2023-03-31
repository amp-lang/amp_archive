# Changelog

## v0.4.0-alpha

### Language
- [feat!: (#32) implement `export` modifier](https://github.com/amp-lang/amp/issues/32)
- [feat: (#29) implement `import` statements](https://github.com/amp-lang/amp/issues/29)
- [feat: (#35) implement `namespace`s](https://github.com/amp-lang/amp/issues/35)
- [feat: (#37) `as` conversion operator](https://github.com/amp-lang/amp/issues/37)
- [feat: (#38) index operator](https://github.com/amp-lang/amp/issues/37)
- [feat: (#38) subslice operator](https://github.com/amp-lang/amp/issues/37)
- feat!: external names for functions
- [feat: (#39) variadic function arguments](https://github.com/amp-lang/amp/issues/39)
- [feat: (#41) type aliases](https://github.com/amp-lang/amp/issues/41)
- [feat!: (#52) unsized types](https://github.com/amp-lang/amp/issues/52)
- [feat!: (#34) export struct fields](https://github.com/amp-lang/amp/issues/34)
- [feat: (#43) `<<` and `>>` operators](https://github.com/amp-lang/amp/issues/43)
- [feat: (#43) bitwise NOT operator (`~`)](https://github.com/amp-lang/amp/issues/43)
- [feat: (#43) bitwise AND operator (`&`)](https://github.com/amp-lang/amp/issues/43)
- [feat: (#43) bitwise XOR operator (`^`)](https://github.com/amp-lang/amp/issues/43)
- [feat: (#43) bitwise PIPE operator (`|`)](https://github.com/amp-lang/amp/issues/43)

### Library
- feat: implement standard library, I guess
- feat: implement `Std.Alloc`, `Std.Realloc` and `Std.Free`.
- feat: implement `Std.Copy`.
- refactor!: remove `runtime/intrinsic.amp`
- feat: implement `Std.Buffer` type
- feat: `C/Stdio` Amp bindings to C's `stdio.h`

### Compiler
- [fix!: (#33) create new scope for each block](https://github.com/amp-lang/amp/issues/33)
- feat: automatically link with standard library and runtime
- fix: local struct variables were always constant
- [fix!: (#46) throw error when invalid syntax is found in root](https://github.com/amp-lang/amp/issues/46)
- [fix!: (#47) throw error when modifiers are before EOF](https://github.com/amp-lang/amp/issues/47)
- [fix!: (#42) disallow private types to be exposed](https://github.com/amp-lang/amp/issues/42)
- [fix: (#49) allow multiple private functions to have the same name](https://github.com/amp-lang/amp/issues/49)
- [fix: (#40) allow multiple declarations of functions](https://github.com/amp-lang/amp/issues/40)
- [feat: (#50) remove support for null terminated strings](https://github.com/amp-lang/amp/issues/50)
- [feat: (#55) implement `run` subcommand](https://github.com/amp-lang/amp/issues/55)

## v0.3.0-alpha

### Language
- [feat (#21): [syntax/scanner, syntax/parser, typechecker, codegen] implement struct types & constructors](https://github.com/amp-lang/amp/issues/21)
- [feat (#22): [syntax/scanner, syntax/parser, typechecker, codegen] implement struct field accessing](https://github.com/amp-lang/amp/issues/22)
- [feat (#24): [typechecker, codegen] assign fields of struct references](https://github.com/amp-lang/amp/issues/24)
- [feat (#25): [typechecker, codegen] reference a struct field](https://github.com/amp-lang/amp/issues/25)
- [feat (#26): [syntax/scanner, syntax/parser, typechecker, codegen] basic math](https://github.com/amp-lang/amp/issues/26)
- [feat (#28): [syntax/scanner, syntax/parser, typechecker, codegen] `while` loops](https://github.com/amp-lang/amp/issues/28)
- [feat (#30): [syntax/scanner, syntax/parser, typechecker, codegen] the `==` operator](https://github.com/amp-lang/amp/issues/30)
- [feat (#30): [syntax/scanner, syntax/parser, typechecker, codegen] the `!=` operator](https://github.com/amp-lang/amp/issues/30)
- [feat (#30): [syntax/scanner, syntax/parser, typechecker, codegen] `<=`, `<`, `>` and `=>` operators](https://github.com/amp-lang/amp/issues/30)
- [feat (#27): [syntax/scanner, syntax/parser, typechecker, codegen] `if` statements](https://github.com/amp-lang/amp/issues/27)

### Compiler
- [feat (#23): implement multiple module linking](https://github.com/amp-lang/amp/issues/23)
- fix: don't return correct struct field
- refactor: remove `unescaper` dependency
- fix: allow function to return nothing

## v0.2.0-alpha

### Language
- [feat (#3): [syntax/parser] implement string escapes](https://github.com/amp-lang/amp/issues/3)
- [feat (#5): [syntax/parser, typechecker, codegen] decimal integer literals](https://github.com/amp-lang/amp/issues/5)
- [feat (#4): [syntax/parser, typechecker, codegen] return statements](https://github.com/amp-lang/amp/issues/4)
- [feat (#8): [syntax/scanner, syntax/parser, typechecker, codegen] implement slice types](https://github.com/amp-lang/amp/issues/8)
- [feat (#9): [syntax/scanner, syntax/parser, typechecker, codegen] implement variable declarations](https://github.com/amp-lang/amp/issues/9)
- [feat (#11): [typechecker, codegen] implement function arguments](https://github.com/amp-lang/amp/issues/11)
- [feat (#12): [typechecker, codegen] implement returning big values](https://github.com/amp-lang/amp/issues/12)
- [feat (#13): [typechecker, codegen] implement function calls as values](https://github.com/amp-lang/amp/issues/13)
- [feat (#14): [typechecker, codegen] implement other integer types](https://github.com/amp-lang/amp/issues/14)
- [feat (#15): [syntax/parser, typechecker, codegen] implement variable assignment](https://github.com/amp-lang/amp/issues/15)
- [feat (#19): [syntax/scanner, syntax/parser, typechecker, codegen] implement dereference operator](https://github.com/amp-lang/amp/issues/19)
- [feat (#20): [typechecker, codegen] implement dereference assignment](https://github.com/amp-lang/amp/issues/20)
- [feat (#6): [syntax/parser] implement hexadecimal, binary and octal literals](https://github.com/amp-lang/amp/issues/6)
- [feat (#16): [syntax/scanner] skip comments in scanner](https://github.com/amp-lang/amp/issues/16)
- fix: allow the new integer types to be used in variables.
- [feat (#18): [syntax/scanner, typechecker, codegen] implement reference operator](https://github.com/amp-lang/amp/issues/18)
- feat: implicitly convert mutable references to immutable references
- [feat (#17): [syntax/scanner, syntax/parser, typechecker, codegen] implement boolean types](https://github.com/amp-lang/amp/issues/17)

### Compiler
- feat: Basic stopwatch benchmark for Amp compile times.
- feat: Wrap linker in `build` subcommand
- feat: Add `-l` linker option to command line.
- [fix (#7): no longer panic if compiler fails](https://github.com/amp-lang/amp/issues/7)

### Internals
- refactor: Make `GenericValue` abstraction to clean up code a bit.

## v0.1.0-alpha
*The starting point for the changelog.*