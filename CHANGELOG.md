# Revision history for libclang-bindings

## ?.?.? -- YYYY-mm-dd

### Breaking changes

### New features

* Add a binding for `clang_isBeforeInTranslationUnit`. This function is only
  available for Clang versions 20.1 and newer; see [PR-53][pr-53].
* Add a binding for the `clang_Type_getOffsetOf` function. See [PR #37][pr-37].
* Add a new `clang_disposeToken` function to free a single `CXToken`. This is a
  helper function alongside the existing `clang_disposeTokens` functions, which
  frees arrays of `CXToken`s. See [PR#42][pr-42].
* Add a new `foldTry` function that behaves like `foldWitHandler`, but it
  returns the caught exception as a value like `Control.Exception.try` would.
  The caught exception is represented using a new type called `FoldException`.
  See [PR #47][pr-47]

### Minor changes

### Bug fixes

[pr-37]: https://github.com/well-typed/libclang/pull/37
[pr-42]: https://github.com/well-typed/libclang/pull/42
[pr-47]: https://github.com/well-typed/libclang/pull/47
[pr-53]: https://github.com/well-typed/libclang/pull/53

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.
