#include "rframe.h"
#include <string.h>


SEXP rframe_split_group(SEXP group_, SEXP ngroup_)
{
    SEXP index_, set_;
    const double *group;
    double **set;
    R_xlen_t g, i, j, n, ngroup, *size;

    n      = XLENGTH(group_);
    group  = REAL(group_);
    ngroup = (R_xlen_t)(REAL(ngroup_)[0]);

    size = (void *)R_alloc(ngroup, sizeof(*size));
    memset(size, 0, ngroup * sizeof(*size));

    for (i = 0; i < n; i++) {
        RFRAME_CHECK_INTERRUPT(i);

        g = (R_xlen_t)(group[i] - 1);
        size[g] += 1;
    }

    PROTECT(index_ = Rf_allocVector(VECSXP, ngroup));
    set = (void *)R_alloc(ngroup, sizeof(*set));

    for (g = 0; g < ngroup; g++) {
        RFRAME_CHECK_INTERRUPT(g);

        set_ = Rf_allocVector(REALSXP, size[g]);
        SET_VECTOR_ELT(index_, g, set_);
        set[g] = REAL(set_);
    }

    memset(size, 0, ngroup * sizeof(*size));

    for (i = 0; i < n; i++) {
        RFRAME_CHECK_INTERRUPT(i);

        g = (R_xlen_t)(group[i] - 1);
        j = size[g];
        set[g][j] = (double)(i + 1);
        size[g] = j + 1;
    }

    UNPROTECT(1);
    return index_;
}
