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
