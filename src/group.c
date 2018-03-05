#include "rframe.h"

R_xlen_t rframe_nrow_dataset(SEXP x_)
{
    SEXP attr_, nrow_;
    R_xlen_t nrow;
    int nprot = 0;

    PROTECT(attr_ = Rf_install("dataset.nrow")); nprot++;
    PROTECT(nrow_ = Rf_getAttrib(x_, attr_)); nprot++;
    PROTECT(nrow_ = Rf_coerceVector(nrow_, REALSXP)); nprot++;
    nrow = (R_xlen_t)REAL(nrow_)[0];
    UNPROTECT(nprot);
    return nrow;
}


SEXP rframe_groups(SEXP x_, SEXP sort_)
{
    R_xlen_t n;
    uint64_t *hash;
    int sort;
   
    n = rframe_nrow_dataset(x_);
    sort = LOGICAL(sort_)[0] == TRUE;

    hash = (void *)R_alloc(n, sizeof(*hash));
    rframe_hash_init(hash, n);
    rframe_hash_dataset(hash, n, x_);

    return R_NilValue;
}
