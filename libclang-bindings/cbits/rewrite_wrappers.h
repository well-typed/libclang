#ifndef REWRITE_WRAPPERS_H
#define REWRITE_WRAPPERS_H

#include "libclang_version.h"

/**
 * Wrappers for the Rewrite API
 */

#if LIBCLANG_VERSION_MAJOR == 21
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif
#include <clang-c/Rewrite.h>
#if LIBCLANG_VERSION_MAJOR == 21
#pragma GCC diagnostic pop
#endif

static inline void wrap_CXRewriter_insertTextBefore(CXRewriter Rew, const CXSourceLocation *Loc, const char *Insert) {
    clang_CXRewriter_insertTextBefore(Rew, *Loc, Insert);
}

#endif
