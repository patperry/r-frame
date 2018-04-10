#include "rframe.h"


static void rframe_subset_logical(SEXP x_, R_xlen_t n, const double *i, int *ans)
{
    R_xlen_t j;
    const int *x;

    x = LOGICAL(x_);
    for (j = 0; j < n; j++) {
        ans[j] = x[(R_xlen_t)(i[j] - 1)];
    }
}


static void rframe_subset_raw(SEXP x_, R_xlen_t n, const double *i, Rbyte *ans)
{
    R_xlen_t j;
    const Rbyte *x;

    x = RAW(x_);
    for (j = 0; j < n; j++) {
        ans[j] = x[(R_xlen_t)(i[j] - 1)];
    }
}


static void rframe_subset_integer(SEXP x_, R_xlen_t n, const double *i, int *ans)
{
    R_xlen_t j;
    const int *x;

    x = INTEGER(x_);
    for (j = 0; j < n; j++) {
        ans[j] = x[(R_xlen_t)(i[j] - 1)];
    }
}


static void rframe_subset_double(SEXP x_, R_xlen_t n, const double *i, double *ans)
{
    R_xlen_t j;
    const double *x;

    x = REAL(x_);
    for (j = 0; j < n; j++) {
        ans[j] = x[(R_xlen_t)(i[j] - 1)];
    }
}


static void rframe_subset_complex(SEXP x_, R_xlen_t n, const double *i, Rcomplex *ans)
{
    R_xlen_t j;
    const Rcomplex *x;

    x = COMPLEX(x_);
    for (j = 0; j < n; j++) {
        ans[j] = x[(R_xlen_t)(i[j] - 1)];
    }
}


static void rframe_subset_character(SEXP x_, R_xlen_t n, const double *i, SEXP ans_)
{
    SEXP elt;
    R_xlen_t j;

    for (j = 0; j < n; j++) {
        elt = STRING_ELT(x_, (R_xlen_t)(i[j] - 1));
        SET_STRING_ELT(ans_, j, elt);
    }
}


static void rframe_subset_list(SEXP x_, R_xlen_t n, const double *i, SEXP ans_)
{
    SEXP elt;
    R_xlen_t j;

    for (j = 0; j < n; j++) {
        elt = VECTOR_ELT(x_, (R_xlen_t)(i[j] - 1));
        SET_VECTOR_ELT(ans_, j, elt);
    }
}


SEXP rframe_subset(SEXP x_, SEXP i_)
{
    SEXP ans_, names_;
    SEXPTYPE type;
    R_xlen_t n;
    const double *i;
    int nprot = 0;

    type = TYPEOF(x_);
    n = XLENGTH(i_);
    i = REAL(i_);

    PROTECT(ans_ = Rf_allocVector(type, n)); nprot++;

    switch (type) {
    case LGLSXP:
        rframe_subset_logical(x_, n, i, LOGICAL(ans_));
        break;

    case RAWSXP:
        rframe_subset_raw(x_, n, i, RAW(ans_));
        break;

    case INTSXP:
        rframe_subset_integer(x_, n, i, INTEGER(ans_));
        break;

    case REALSXP:
        rframe_subset_double(x_, n, i, REAL(ans_));
        break;

    case CPLXSXP:
        rframe_subset_complex(x_, n, i, COMPLEX(ans_));
        break;

    case STRSXP:
        rframe_subset_character(x_, n, i, ans_);
        break;

    case VECSXP:
        rframe_subset_list(x_, n, i, ans_);
        break;

    default:
        Rf_error("invalid subset type");
    }

    names_ = Rf_getAttrib(x_, R_NamesSymbol);
    if (names_ != R_NilValue) {
        PROTECT(names_); nprot++;
        Rf_setAttrib(x_, R_NamesSymbol, rframe_subset(names_, i_));
    }

    UNPROTECT(nprot);
    return ans_;
}
