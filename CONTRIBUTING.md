# Contributing

## Installation requirements

The following should be installed:

* The `libclang` C library from the [The LLVM
  project](https://github.com/llvm/llvm-project)

* `autoreconf` and its dependencies
  * Ubuntu:
    ```
    apt-get install build-essential
    ```
  * MacOS:
    ```
    brew install coreutils autoconf automake
    ```
  * Windows (MSYS2):
    ```
    pacman --noconfirm -Sy base-devel automake-wrapper autoconf-wrapper
    ```

## Building

The project is built using `autoreconf`, `ghc` and `cabal`.

```
autoreconf -i
cabal update
cabal run clang-bootstrap
cabal build all
```

`autoreconf` should be invoked manually to generate a configuration script for
the Haskell library. The configuration script is run automatically when `cabal
build` is invoked.

`clang-bootstrap` should be invoked manuall to generate C wrapper functions and
Haskel foreign import declarations for the `libclang` C library. This code is
automatically built when `cabal build` is invoked.

## Testing

Tests are run using `cabal`.

```
cabal build all
cabal test all
```

## Code style

There is no strict code style, but try to keep the code style consistent
throughout the repository and favour readability. Code should be well-documented
and well-tested.

## Formatting

We use `stylish-haskell` to format Haskell files, and we use `cabal-fmt` to
format `*.cabal` files. See the helpful scripts in the [scripts
folder](./scripts/), and the [`stylish-haskell` configuration
file](./.stylish-haskell.yaml).

To perform a pre-commit code formatting pass, run one of the following:

```
./scripts/ci/format-cabal-fmt.sh
./scripts/ci/format-stylish-haskell.sh
```

## Pull requests

The following are requirements for merging a PR into `main`:
* Each commit should be small and should preferably address one thing. Commit
  messages should be useful.
* Document and test your changes.
* The PR should have a useful description, and it should link issues that it
  resolves (if any).
* Changes introduced by the PR should be recorded in the relevant changelog
  files. Ideally, each changelog entry should link to the PR that introduced the
  changes, and it should be placed in the relevant category (e.g., breaking
  changes, new features).
* PRs should not bundle many unrelated changes.
* The PR should pass all CI checks.

## Releases

Releases follow the [Haskell Package Versioning
Policy](https://pvp.haskell.org/). We use version numbers consisting of 3 parts,
like `A.B.C`.
* `A.B` is the *major* version number. A bump indicates a breaking change.
* `C` is the *minor* version number. A bump indicates a non-breaking change.

To publish a release for a package, follow the steps below:

* Changelog checks (`CHANGELOG.md`):
  * Check that all user-facing changes have been recorded.
  * Check that each changelog entry is in the correct category.
  * Check that each changelog entry links to a PR, if applicable.
  * Add or update the changelog's section header with the package version that
    is going to be released, and the date of the release. The version should be
    picked based on our package versioning policy.

* Cabal file checks (`*.cabal`):
  * Update the `version` field.
  * Update the `tag` field of the `source-repository this` stanza.

* Cabal project file checks (`cabal.project*`):
  * Update the `index-state` in the `cabal.project` file to the current
    date-time, or the closest valid date-time to the current date-time, so that
    CI builds and tests the libraries with the newest versions of dependencies.
