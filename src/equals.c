#include "rframe.h"
#include <assert.h>
#include <string.h>


static int rframe_equals_column(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2);
static int rframe_equals_logical(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2);
static int rframe_equals_raw(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2);
static int rframe_equals_integer(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2);
static int rframe_equals_double(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2);
static int rframe_equals_complex(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2);
static int rframe_equals_character(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2);


int rframe_equals_dataset(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2)
{
    SEXP col1, col2;
    R_xlen_t j, ncol;

    ncol = XLENGTH(x1_);
    for (j = 0; j < ncol; j++) {
        col1 = VECTOR_ELT(x1_, j);
        col2 = (x1_ == x2_) ? col1 : VECTOR_ELT(x2_, j);
        if (!rframe_equals_column(col1, i1, col2, i2))
            return 0;
    }

    return 1;
}


int rframe_equals_column(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2)
{
    int t = TYPEOF(x1_);

    switch (t) {
    case LGLSXP:
        return rframe_equals_logical(x1_, i1, x2_, i2);

    case RAWSXP:
        return rframe_equals_raw(x1_, i1, x2_, i2);

    case INTSXP:
        return rframe_equals_integer(x1_, i1, x2_, i2);

    case REALSXP:
        return rframe_equals_double(x1_, i1, x2_, i2);

    case CPLXSXP:
        return rframe_equals_complex(x1_, i1, x2_, i2);

    case STRSXP:
        return rframe_equals_character(x1_, i1, x2_, i2);

    case VECSXP:
        return rframe_equals_dataset(x1_, i1, x2_, i2);

    default:
        assert(t == NILSXP);
        return 1;
    }
}


int rframe_equals_logical(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2)
{
    const int *x1 = LOGICAL(x1_);
    const int *x2 = (x1_ == x2_) ? x1 : LOGICAL(x2_);
    return x1[i1] == x2[i2];
}


int rframe_equals_raw(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2)
{
    const Rbyte *x1 = RAW(x1_);
    const Rbyte *x2 = (x1_ == x2_) ? x1 : RAW(x2_);
    return x1[i1] == x2[i2];
}


int rframe_equals_integer(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2)
{
    const int *x1 = INTEGER(x1_);
    const int *x2 = (x1_ == x2_) ? x1 : INTEGER(x2_);
    return x1[i1] == x2[i2];
}


static int equals_double(double x1, double x2)
{
    union {
        double d;
        uint64_t u;
    } value1, value2;
    value1.d = x1;
    value2.d = x2;
    return (value1.u == value2.u);
}


int rframe_equals_double(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2)
{
    const double *x1 = REAL(x1_);
    const double *x2 = (x1_ == x2_) ? x1 : REAL(x2_);
    return equals_double(x1[i1], x2[i2]);
}


// Diverge from the R behavior, which considers NA+i and NA to be equal
int rframe_equals_complex(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2)
{
    const Rcomplex *x1 = COMPLEX(x1_);
    const Rcomplex *x2 = (x1_ == x2_) ? x1 : COMPLEX(x2_);
    return (equals_double(x1[i1].r, x2[i2].r)
            && equals_double(x1[i1].i, x2[i2].i));
}


int rframe_equals_character(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2)
{
    SEXP x1, x2;
    R_xlen_t n1, n2;
    const char *s1, *s2;
    cetype_t ce1, ce2;
    int eq, cmp, nprot = 0;

    PROTECT(x1 = STRING_ELT(x1_, i1)); nprot++;
    PROTECT(x2 = STRING_ELT(x2_, i2)); nprot++;

    // inputs are normalized, so encoding is either ASCII, UTF-8, or bytes.
    //
    // could optimize the comparison to `eq = (x1 == x2)`, but that relies on R
    // internals

    if (x1 == x2) {
        eq = 1;
    } else {
        n1 = XLENGTH(x1);
        n2 = XLENGTH(x2);
        if (n1 != n2) {
            eq = 0;
        } else {
            s1 = CHAR(x1);
            s2 = CHAR(x2);
            cmp = memcmp(s1, s2, n1);
            if (cmp == 0) {
                ce1 = Rf_getCharCE(x1);
                ce2 = Rf_getCharCE(x2);
                eq = (ce1 == ce2);
            } else {
                eq = 0;
            }
        }
    }

    UNPROTECT(nprot);
    return eq;
}
