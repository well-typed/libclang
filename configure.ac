AC_PREREQ([2.71])
AC_INIT([clang],[0.1.0])

dnl --with-compiler argument: This argument is passed by Cabal, so we have to
dnl handle it.  We do not use it, so we ignore the passed value.
AC_ARG_WITH(
  [compiler],
  [AS_HELP_STRING([--with-compiler], [Haskell compiler])]
)

dnl --with-so argument: This argument allows users to specify the path to a
dnl shared library file, used to determine extra-libraries and extra-lib-dirs
dnl on Linux systems.  It is used to work around Cabal linking issues.
dnl
dnl If specified, $wt_user_lib_dir and $wt_user_lib are set.
wt_with_so_path='NOT_SET'
AC_ARG_WITH(
  [so],
  [AS_HELP_STRING([--with-so=PATH], [shared library to link to])],
  [wt_with_so_path=$withval],
  []
)
case "$wt_with_so_path" in
  /*)
    if test ! -f "$wt_with_so_path" ; then
      AC_MSG_ERROR([--with-so file not found: $wt_with_so_path])
    fi
    wt_user_lib_dir="$(dirname "$wt_with_so_path")"
    AC_MSG_NOTICE([with library directory: $wt_user_lib_dir])
    wt_with_so_filename="$(basename "$wt_with_so_path")"
    wt_with_so_lib_1="${wt_with_so_filename#lib}"
    if test "$wt_with_so_lib_1" = "$wt_with_so_filename" ; then
      AC_MSG_ERROR([--with-so file does not have lib prefix: $wt_with_so_path])
    fi
    wt_user_lib="${wt_with_so_lib_1%.so}"
    if test "$wt_user_lib" = "$wt_with_so_lib_1" ; then
      AC_MSG_ERROR([--with-so file does not have .so suffix: $wt_with_so_path])
    fi
    AC_MSG_NOTICE([with library: $wt_user_lib])
    ;;
  'NOT_SET')
    ;;
  '')
    AC_MSG_ERROR([--with-so specified without an argument])
    ;;
  *)
    AC_MSG_ERROR([--with-so path not absolute: $wt_with_so_path])
    ;;
esac

dnl LLVM_CONFIG variable: Users may use this to specify which llvm-config
dnl executable to use.
AC_ARG_VAR(LLVM_CONFIG, [Location of llvm-config])
AC_PATH_PROG([LLVM_CONFIG],[llvm-config],[])

dnl LLVM_PATH variable: Users may alternatively specify the location of
dnl an LLVM installation.
AC_ARG_VAR(LLVM_PATH, [Location of LLVM installation])

dnl LLVM resolution:
dnl 1. Use llvm-config specified by LLVM_CONFIG if set
dnl 2. Use llvm-config in PATH if found
dnl 3. Use LLVM_PATH if set
dnl 4. Fail
AC_MSG_CHECKING([LLVM resolution mode])
if test -n "$LLVM_CONFIG" ; then
  AC_MSG_RESULT([llvm-config])
else
  if test -n "$LLVM_PATH" ; then
    AC_MSG_RESULT([LLVM_PATH=$LLVM_PATH])
  else
    AC_MSG_ERROR([llvm-config not found and LLVM_PATH not set])
  fi
fi

AC_MSG_CHECKING([LLVM version])
if test -n "$LLVM_CONFIG" ; then
  dnl This checks that we can run llvm-config
  wt_llvm_version="$("$LLVM_CONFIG" --version)"
  if test $? == 0 ; then
    AC_MSG_RESULT([$wt_llvm_version])
  else
    AC_MSG_RESULT([error])
    AC_MSG_ERROR([could not run llvm-config])
  fi
else
  AC_MSG_RESULT([skipped])
fi

dnl Clang library resolution:
dnl 1. Use user-specified library if --with-so is used
dnl 2. Use clang
AC_MSG_CHECKING([Clang library])
if test -n "$wt_user_lib" ; then
  wt_clang_lib="$wt_user_lib"
else
  wt_clang_lib='clang'
fi
AC_MSG_RESULT([$wt_clang_lib])
AC_SUBST([CLANG_LIB], ["$wt_clang_lib"])
dnl LIBS environment variable is set for checks below
LIBS="${LIBS} -l${wt_clang_lib}"

dnl LLVM library directory resolution:
dnl 1. Use llvm-config or LLVM_PATH to determine the real library directory
dnl 2. Append user-specified library directory if --with-so is used
AC_MSG_CHECKING([LLVM library directories])
if test -n "$LLVM_CONFIG" ; then
  wt_clang_lib_dirs="$("$LLVM_CONFIG" --libdir)"
else
  wt_clang_lib_dirs="${LLVM_PATH}/lib"
fi
dnl LDFLAGS environment variable is set for checks below
if test -n "$wt_user_lib_dir" && test "$wt_user_lib_dir" != "$wt_clang_lib_dirs"; then
  LDFLAGS="${LDFLAGS} -L${wt_user_lib_dir} -L${wt_clang_lib_dirs}"
  wt_clang_lib_dirs="${wt_user_lib_dir} ${wt_clang_lib_dirs}"
else
  LDFLAGS="${LDFLAGS} -L${wt_clang_lib_dirs}"
fi
AC_MSG_RESULT($wt_clang_lib_dirs)
AC_SUBST([CLANG_LIB_DIRS], ["$wt_clang_lib_dirs"])

dnl LLVM include directory resolution logic:
dnl 1. Use llvm-config or LLVM_PATH to determine the include directory
AC_MSG_CHECKING([LLVM include directory])
if test -n "$LLVM_CONFIG" ; then
  wt_clang_include_dir="$("$LLVM_CONFIG" --includedir)"
else
  wt_clang_include_dir="${LLVM_PATH}/include"
fi
dnl CPPFLAGS environment variable is set for checks below
CPPFLAGS="${CPPFLAGS} -I${wt_clang_include_dir}"
AC_MSG_RESULT($wt_clang_include_dir)
AC_SUBST([CLANG_INCLUDE_DIR], ["$wt_clang_include_dir"])

dnl Check C compiler needed for further checks
AC_PROG_CC

AC_CHECK_HEADER(
  [clang-c/Index.h],
  [],
  [AC_MSG_ERROR([Cannot find libclang headers])]
)

AC_CHECK_LIB(
  [$wt_clang_lib],
  [clang_createIndex],
  [],
  [AC_MSG_ERROR([Cannot link against libclang])]
)

dnl Get the version of the linked libclang shared library.  This is subsituted
dnl into a generated Haskell module to provide the "compile-time" version.
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
  [wt_libclang_bindings_version=$(./conftest$EXEEXT)],
  [AC_MSG_FAILURE([Unable to determine libclang version])]
)
AC_MSG_RESULT($wt_libclang_bindings_version)
dnl TODO Check version for consistency
dnl https://www.gnu.org/software/autoconf-archive/ax_compare_version.html
AC_SUBST(
  [LIBCLANG_BINDINGS_VERSION],
  ["$(echo ${wt_libclang_bindings_version} | sed 's/\"/\\\"/g')"]
)

dnl clang_isBeforeInTranslationUnit was introduced in Clang version 20.1, but
dnl CINDEX_VERSION_MINOR was not bumped, so we need to check for it.
AC_CHECK_FUNCS([clang_isBeforeInTranslationUnit])

dnl Output
AC_CONFIG_HEADERS([autogen/clang_config.h])
AC_CONFIG_FILES([clang.buildinfo autogen/Version_libclang_bindings.hs])
AC_OUTPUT
