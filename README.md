<p align="center"><img src="assets/logo.svg" width="60%"></p>
<p align="center">A language designed for building high performance systems.</p>

| Platform                | Support                |
| ----------------------- | ---------------------- |
| `x86_64-pc-windows`     | :white_check_mark:     |
| `x86_64-unknown-linux`  | :warning: **untested** |
| `x86_64-unknown-darwin` | :warning: **untested** |
| `aarch64-unknown-linux` | :warning: **untested** |

## Overview
> :warning: For legal reasons, I must warn you that the bold claims in this section aren't necessarily true, *yet*.
>
> :exclamation: Also, Amp isn't really ready for serious use yet.  It's not well tested and there's probably some undetected bugs in the compiler.  With that in mind, have fun! :)

Amp focuses on:

- **Performance** :zap: Amp's performance rivals that of C and C++.
- **Simplicity** :art: Amp focuses on less, but more powerful features.
- **Speedy compilation** :motorcycle: Amp's simple design enables competitive compile times.

```amp
import "Std";

export func Main() {
    Std.Println("Hello, world!");
}
```

### Contributing
Since Amp is vastly under-documented, feel free to [create an issue](https://github.com/amp-lang/amp/issues/new) if you have any questions about the project.  If you are looking for something to contribute to the project, check out the [open issues](https://github.com/amp-lang/amp/issues?q=is%3Aissue+is%3Aopen).  Thanks for taking the time to contribute! :tada:

We follow the [Contributor Covenant](https://www.contributor-covenant.org/version/2/1/code_of_conduct/) code of conduct (tl;dr: have some basic common sense and be respectful).
<!-- Amp is mostly a one man project for now, but if you'd like to help with anything, feel free to browse through the [open issues](https://github.com/amp-lang/amp/issues?q=is%3Aissue+is%3Aopen) (I use them to keep track of what I need to do).  Anything marked as a "proposal" is a language design decision open for conversation until I decide what to do with it.  Otherwise, feel free to ask about an issue if you have any questions. -->

### License
Amp is licensed under the MIT license.

## Getting Started
You can install Amp from a [prebuilt binary for your system](https://github.com/amp-lang/amp/releases) or you can use Cargo to install it:

```sh
cargo install ampc
amp -V
```

Building an executable with Amp requires an external linker (as of right now, it's hardcoded as GCC, meaning GCC is required to build an executable):

```sh
amp test.amp -o test
./test # => Hello, world!
```