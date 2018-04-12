#include "rframe.h"
#include <stddef.h>
#include <R_ext/Rdynload.h>

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}


static const R_CallMethodDef CallEntries[] = {
    CALLDEF(rframe_as_simple_double, 1),
    CALLDEF(rframe_rowid_keyset, 3),
    CALLDEF(rframe_split_group, 2),
    CALLDEF(rframe_subset, 2),
    CALLDEF(rframe_subscript, 4),
    CALLDEF(rframe_unique, 1),
    {NULL, NULL, 0}
};


void R_init_frame(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
