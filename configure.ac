AC_PREREQ([2.69])
AC_INIT([clang],[0.1.0])
dnl AC_CONFIG_MACRO_DIRS([m4])

AC_PROG_CC

dnl we get --with-compiler warnings without this
dnl this is not great, as really it should be --with-ghc or --with-readline
dnl i.e. pointing to the external software by name
AC_ARG_WITH([compiler],
  [AS_HELP_STRING([--with-compiler], [Haskell compiler])])

dnl The discovery logic is following
dnl * look for llvm-config (LLVM_CONFIG if set, in PATH otherwise)
dnl * if not found
dnl   - if LLVM_PATH is set, use it (skip version check)
dnl   - otherwise fail.

AC_ARG_VAR(LLVM_CONFIG, [Location of llvm-config])
AC_ARG_VAR(LLVM_PATH, [Location of LLVM installation])

AC_PATH_PROG([LLVM_CONFIG],[llvm-config],[])
if test -z "$LLVM_CONFIG"; then
  if test -z "$LLVM_PATH"; then
    AC_MSG_ERROR([could not find llvm-config; LLVM_PATH is also not set])
  else
    AC_MSG_NOTICE([could not find llvm-config; using LLVM_PATH=$LLVM_PATH])
  fi
fi

dnl This also checks that we can run LLVM_CONFIG
AC_MSG_CHECKING([LLVM version])

if test -z "$LLVM_CONFIG"; then
  AC_MSG_RESULT([skipped])
else
LLVM_VERSION="$("$LLVM_CONFIG" --version)"
if test $? == 0; then
  AC_MSG_RESULT([$LLVM_VERSION])
  dnl TODO: Use https://www.gnu.org/software/autoconf-archive/ax_compare_version.html to compare version
else
  AC_MSG_RESULT([error])
  AC_MSG_ERROR([could not run llvm-config])
fi
fi

dnl LIBS/LIBDIR/INCLUDEDIR

CLANG_EXTRA_LIBS="clang"

AC_MSG_CHECKING([LLVM library directory])
if test -z "$LLVM_CONFIG"; then
CLANG_LIBDIR="$LLVM_PATH/lib"
else
CLANG_LIBDIR="$("$LLVM_CONFIG" --libdir)";
fi
AC_MSG_RESULT($CLANG_LIBDIR)

AC_MSG_CHECKING([LLVM include directory])
if test -z "$LLVM_CONFIG"; then
CLANG_INCLUDEDIR="$LLVM_PATH/include"
else
CLANG_INCLUDEDIR="$("$LLVM_CONFIG" --includedir)"
fi
AC_MSG_RESULT($CLANG_INCLUDEDIR)

dnl Runtime checks

dnl Set some C related environment variables for further tests
LIBS="$LIBS -l$CLANG_EXTRA_LIBS"
LDFLAGS="$LDFLAGS -L$CLANG_LIBDIR"
CPPFLAGS="$CPPFLAGS -I$CLANG_INCLUDEDIR"

AC_CHECK_HEADER([clang-c/Index.h],[],[AC_MSG_ERROR([Cannot find libclang headers])])
AC_CHECK_LIB([clang], [clang_createIndex],[],[AC_MSG_ERROR([Cannot link against libclang])])

AC_MSG_CHECKING([libclang version])
AC_LINK_IFELSE(
  [AC_LANG_PROGRAM(
    [[
      #include <clang-c/Index.h>
      #include <stdio.h>
    ]],
    [[
      CXString cxstring = clang_getClangVersion();
      printf("%s\n", clang_getCString(cxstring));
      clang_disposeString(cxstring);
    ]]
  )],
  [LIBCLANG_BINDINGS_VERSION=$(./conftest$EXEEXT)],
  [AC_MSG_FAILURE([Unable to determine libclang version])]
)
AC_MSG_RESULT($LIBCLANG_BINDINGS_VERSION)

dnl Substitution

AC_SUBST([CLANG_EXTRA_LIBS])
AC_SUBST([CLANG_LIBDIR])
AC_SUBST([CLANG_INCLUDEDIR])
AC_SUBST(
  [LIBCLANG_BINDINGS_VERSION],
  ["$(echo ${LIBCLANG_BINDINGS_VERSION} | sed 's/\"/\\\"/g')"]
)

AC_CONFIG_FILES([clang.buildinfo autogen/Version_libclang_bindings.hs])

AC_OUTPUT
