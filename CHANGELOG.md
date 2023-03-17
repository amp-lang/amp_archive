# Changelog

## v0.3.0-alpha

## Language
- [feat (#21): [syntax/scanner, syntax/parser, typechecker, codegen] implement struct types & constructors](https://github.com/amp-lang/amp/issues/21)
- [feat (#23): [cli] implement multiple module linking](https://github.com/amp-lang/amp/issues/23)

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