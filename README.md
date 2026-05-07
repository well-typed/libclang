# `libclang-bindings`: Haskell bindings for the libclang C library

> [!WARNING]
> This project is under active development, and has not yet been released.
> That said, you might already get some usable results out of it; if something
> breaks, please check the issue tracker to see if the problem is already known,
> and open an issue if not.

TODO <https://github.com/well-typed/libclang/issues/1>: Rewrite the README

## Configuration

This package is configured using an `autoconf` configure script, which
determines how the `libclang` shared library is linked.

## Using `llvm-config`

In general, `llvm-config` is used to determine the include and library
directories.  If you have more than one version LLVM/Clang installed, you can
configure your `PATH` to ensure that the `llvm-config` for the version that you
want to use is found.

Alternatively, you can configure the `LLVM_CONFIG` environment variable to the
path of the `llvm-config` for the version that you want to use.

## Using `LLVM_PATH`

If `llvm-config` is not available, you can instead configure the `LLVM_PATH`
environment variable to determine the include (`$LLVM_PATH/include`) and library
(`$LLVM_PATH/lib`) directories.

> [!NOTE]
> `llvm-config` is available on all systems that we test with.  Windows users
> should use LLVM/Clang as installed by MSYS2, such as the version installed
> with GHC.  LLVM/Clang versions installed from official releases or with Visual
> Studio target MSVC, which is not supported by GHC, and are therefore not
> suitable.

## Cabal linking issue (Linux)

There are cases where Cabal links to the wrong `libclang` shared library when
multiple versions are available.

### Background

The configure script determines the library name and library directory to use
when linking.  The library name, passed to the linker using a `-l` option, is
`clang` by default.  The library directory, passed to the linker using a `-L`
option, is determined using `llvm-config` or `LLVM_PATH` as described above.

The linker finds the `clang` library by searching its library path for shared
library named `libclang.so`.  When there are multiple versions of LLVM/Clang
available, the order of directories in the library path determines which one
is found.

Typically, `libclang.so` is a symbolic link to a file that specifies the full
version, such as `/usr/lib/llvm-21/lib/libclang.so.21.1.8`.  This file has an
embedded string called the `SONAME`, which instructs the linker to link against
a _different_ file instead.  Current versions of LLVM/Clang configure the
`SONAME` to omit the patch number.

| File                                      | Link To              | `SONAME`           |
| :---                                      | :---                 | :---               |
| `/usr/lib/llvm-21/lib/libclang.so`        | `libclang.so.21.1.8` |                    |
| `/usr/lib/llvm-21/lib/libclang.so.21.1.8` |                      | `libclang.so.21.1` |
| `/usr/lib/llvm-21/lib/libclang.so.21.1`   | `libclang.so.21.1.8` |                    |

It is this third file (`libclang.so.21.1`) that we actually end up linking
against.  The resulting library will then continue to work even if the patch is
upgraded (to 21.1.9, say).

Just the filename is linked, _not_ an absolute path.  At runtime, the loader
must find shared library `libclang.so.21.1` in the loader library path.

### Cabal issue

When building a library or executable, Cabal links libraries needed by
transitive dependencies.  A library or executable that (directly or indirectly)
depends on `libclang-bindings` is therefore also linked to `libclang`.  The
linking options configured for `libclang-bindings` are stored in the package
database, and they used again when building the library or executable that
depends on `libclang-bindings`.  The linker options for other dependencies are
also used, however, and the ordering of those options may result in a shared
library for a _different_ version of LLVM/Clang being linked.  The order of
those options is determined by Cabal and cannot be configured.  This results in
_multiple_ versions of `libclang` being linked: the desired version via
`libclang-bindings` and an incorrect version that is linked directly.

For example, consider the case where the `PATH` environment variable is
configured to use LLVM/Clang 21 on a system where LLVM/Clang 22 is the default.
The configure script uses `llvm-config` to determine that library directory
`/usr/lib/llvm-21/lib` should be used.  The `libclang-bindings` library is
linked with options `-lclang` and `-L/usr/lib/llvm-21/lib`, and the linker
resolves `libclang.so` to shared library `libclang.so.21.1` as described above.

A separate executable that depends on `libclang-bindings` may also have a
dependency with library directory `/usr/lib`.  If the `-L/usr/lib` option is
passed before the `-L/usr/lib/llvm-21/lib` option, then `libclang.so` is
resolved differently:

| File                          | Link To              | `SONAME`           |
| :---                          | :---                 | :---               |
| `/usr/lib/libclang.so`        | `libclang.so.22.1.4` |                    |
| `/usr/lib/libclang.so.22.1.4` |                      | `libclang.so.22.1` |
| `/usr/lib/libclang.so.22.1`   | `libclang.so.22.1.4` |                    |

The executable is linked to `libclang.so.22.1` directly as well as
`libclang.so.21.1` via `libclang-bindings`.

### Workaround

The workaround is to use a symbolic link that has a unique name so that it can
be unambiguously resolved, independent of any other linker flags.  To support
this workflow, the `libclang-bindings` configure script has a `--with-so` option
to specify the absolute path to such a symbolic link.  The directory part is
used as the library directory, and the filename is used to determine the library
name (by stripping the `lib` prefix and `.so` suffix).

For example, you could create a symbolic link in directory `~/.cabal/sys-libs`:

| File                                                   | Link To                                 |
| :---                                                   | :---                                    |
| `/home/me/.cabal/sys-libs/libclang_workaround_21_1.so` | `/usr/lib/llvm-21/lib/libclang.so.21.1` |

Use of this symbolic link is configured in a `cabal.project.local` file as
follows:

```
package libclang-bindings
  configure-options: --with-so=/home/me/.cabal/sys-libs/libclang_workaround_21_1.so
```

The configure script set library name `clang_workaround_21_1` and library
directory `/home/me/.cabal/sys-libs`.  The shared library for the correct
version of LLVM/Clang is always linked because `clang_workaround_21_1` always
resolves via the configured symbolic link due to the unique name.  A preceding
`-L/usr/lib` option is no longer problematic.

> [!NOTE]
> Such symbolic links must be persistent.  The library directory is cached in
> the package database and used whenever a package that (transitively) depends
> on `libclang-bindings` is (re)built.

### Upgrading LLVM/Clang

If you upgrade LLVM/Clang (other than patch releases), you must reconfigure and
rebuild all packages that depend on it.  When using the `--with-so` option, you
must also create a new symbolic link to the new shared library.  Note that
`configure-options` are included in the Cabal store hash for the package, so
changing the `--with-so` configuration is sufficient to let Cabal know that a
new build is needed.
