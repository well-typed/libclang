# Revision history for libclang-bindings

## ?.?.? -- YYYY-mm-dd

### Breaking changes

### New features

* Add a binding for the `clang_Type_getOffsetOf` function. See [PR #37][pr-37].
* Add a new `clang_disposeToken` function to free a single `CXToken`. This is a
  helper function alongside the existing `clang_disposeTokens` functions, which
  frees arrays of `CXToken`s. See [PR#42][pr-42].

### Minor changes

### Bug fixes

[pr-37]: https://github.com/well-typed/libclang/pull/37
[pr-42]: https://github.com/well-typed/libclang/pull/42

## 0.1.0-alpha -- 2026-02-06

* First version. Released on an unsuspecting world.