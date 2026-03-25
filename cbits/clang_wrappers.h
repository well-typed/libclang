#ifndef CLANG_WRAPPERS_H
#define CLANG_WRAPPERS_H

#include <clang-c/Index.h>
#include <stdio.h>
#include "clang_config.h"
#include "clang_wrappers_ffi.h"

/**
 * Wrappers for clang functions that take structs, or return them, by value.
 *
 * For functions that return structs by value, we instead expect a buffer to be
 * preallocated Haskell-side.
 */

/**
 * Traversing the AST with cursors
 *
 * NOTE: The visitor is passed the two cursors as pointers, but those pointers
 * are pointers to the /stack/. If these pointers can outlive their scope, then
 * the visitor should copy them to the heap.
 */

typedef enum CXChildVisitResult(*WrapCXCursorVisitor)(CXCursor* cursor, CXCursor* parent);

enum CXChildVisitResult wrap_visitor(CXCursor cursor, CXCursor parent, CXClientData client_data);

static inline unsigned wrap_visitChildren(const CXCursor* parent, WrapCXCursorVisitor visitor) {
    return clang_visitChildren(*parent, &wrap_visitor, visitor);
}

/**
 * Type information for CXCursors
 */

static inline enum CXTypeKind wrap_cxtKind(const CXType* type) {
    return type->kind;
}

static inline signed int wrap_compareTypes(const CXType *A, const CXType *B) {
    if (A->data[0] < B->data[0]) {
        return -1;
    } else if (A->data[0] > B->data[0]) {
        return +1;
    } else {
        if (A->data[1] < B->data[1]) {
            return -1;
        } else if(A->data[1] > B->data[1]) {
            return +1;
        } else {
            return 0;
        }
    }
}

/**
 * Call `clang_getUnqualifiedType`
 *
 * This function does not exist in versions before Clang 16.  This function acts
 * as a no-op in that case, and `result` should not be used.
 *
 * Calling this function with an invalid CT results in a segfault.
 */
static inline void wrap_getUnqualifiedType(const CXType* CT, CXType* result) {
    #if CINDEX_VERSION_MINOR >= 63
        *result = clang_getUnqualifiedType(*CT);
    #endif
}

/**
 * Debugging
 */

void clang_breakpoint(void);

#endif
