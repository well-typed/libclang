# `libclang-bindings`

[![Build Status](https://github.com/well-typed/libclang-bindings/actions/workflows/haskell.yml/badge.svg)](https://github.com/well-typed/libclang-bindings/actions)
[![License: BSD-3-Clause](https://img.shields.io/badge/license-BSD--3--Clause-lightgray.svg)](https://github.com/well-typed/hs-bindgen/blob/main/hs-bindgen/LICENSE)

`libclang-bindings` is a [Haskell][] library that provides bindings for the
[LLVM/Clang][] `libclang` C API.  It supports the [`hs-bindgen`][] project but
can be used independently.

> [!WARNING]
> This project has not had an official release yet.  There is a wide variety of
> C (and C preprocessor) code in the world, so we are currently soliciting
> feedback prior to the first official release of [`hs-bindgen`][].  Please try
> it out!  If something breaks, please check the [issues][] to see if the
> problem is already known, and open an issue if not.

## Documentation

* [`libclang-bindings` manual][]

## Packages in this repository

* [`libclang-bootstrap`][] is used to generate C wrapper functions and Haskell
  foreign import declarations for a subset of the `libclang` C API.
* [`libclang-bindings`][] implements the actual bindings.

## Contribution

Our thanks go to those who have contributed to this project with development,
bug reports, feature requests, blog posts, etc.  We list [contributors][] in
the [`hs-bindgen`][] README.

Please see [`CONTRIBUTING.md`][] for information about contributing to this
project.

[`CONTRIBUTING.md`]: <CONTRIBUTING.md>
[contributors]: <https://github.com/well-typed/hs-bindgen#contributors>
[Haskell]: <https://www.haskell.org/>
[issues]: <https://github.com/well-typed/libclang-bindings/issues>
[`libclang-bindings`]: <libclang-bindings>
[`libclang-bindings` manual]: <manual/README.md>
[`libclang-bootstrap`]: <libclang-bootstrap>
[LLVM/Clang]: <https://github.com/llvm/llvm-project>
[`hs-bindgen`]: <https://github.com/well-typed/hs-bindgen>
