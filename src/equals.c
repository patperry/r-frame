#include "rframe.h"
#include <assert.h>
#include <string.h>


static int rframe_equals_column(SEXP x_, R_xlen_t i1, R_xlen_t i2);
static int rframe_equals_logical(SEXP x_, R_xlen_t i1, R_xlen_t i2);
static int rframe_equals_raw(SEXP x_, R_xlen_t i1, R_xlen_t i2);
static int rframe_equals_integer(SEXP x_, R_xlen_t i1, R_xlen_t i2);
static int rframe_equals_double(SEXP x_, R_xlen_t i1, R_xlen_t i2);
static int rframe_equals_complex(SEXP x_, R_xlen_t i1, R_xlen_t i2);
static int rframe_equals_character(SEXP x_, R_xlen_t i1, R_xlen_t i2);


int rframe_equals_dataset(SEXP x_, R_xlen_t i1, R_xlen_t i2)
{
    SEXP col;
    R_xlen_t j, ncol;

    ncol = XLENGTH(x_);
    for (j = 0; j < ncol; j++) {
        col = VECTOR_ELT(x_, j);
        if (!rframe_equals_column(col, i1, i2))
            return 0;
    }

    return 1;
}


int rframe_equals_column(SEXP x_, R_xlen_t i1, R_xlen_t i2)
{
    int t = TYPEOF(x_);

    switch (t) {
    case LGLSXP:
        return rframe_equals_logical(x_, i1, i2);

    case RAWSXP:
        return rframe_equals_raw(x_, i1, i2);

    case INTSXP:
        return rframe_equals_integer(x_, i1, i2);

    case REALSXP:
        return rframe_equals_double(x_, i1, i2);

    case CPLXSXP:
        return rframe_equals_complex(x_, i1, i2);

    case STRSXP:
        return rframe_equals_character(x_, i1, i2);

    case VECSXP:
        return rframe_equals_dataset(x_, i1, i2);

    default:
        assert(t == NILSXP);
        return 1;
    }
}


int rframe_equals_logical(SEXP x_, R_xlen_t i1, R_xlen_t i2)
{
    const int *x = LOGICAL(x_);
    return x[i1] == x[i2];
}


int rframe_equals_raw(SEXP x_, R_xlen_t i1, R_xlen_t i2)
{
    const Rbyte *x = RAW(x_);
    return x[i1] == x[i2];
}


int rframe_equals_integer(SEXP x_, R_xlen_t i1, R_xlen_t i2)
{
    const int *x = INTEGER(x_);
    return x[i1] == x[i2];
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


int rframe_equals_double(SEXP x_, R_xlen_t i1, R_xlen_t i2)
{
    const double *x = REAL(x_);
    return equals_double(x[i1], x[i2]);
}


// Diverge from the R behavior, which considers NA+i and NA to be equal
int rframe_equals_complex(SEXP x_, R_xlen_t i1, R_xlen_t i2)
{
    const Rcomplex *x = COMPLEX(x_);
    return (equals_double(x[i1].r, x[i2].r)
            && equals_double(x[i1].i, x[i2].i));
}


int rframe_equals_character(SEXP x_, R_xlen_t i1, R_xlen_t i2)
{
    R_xlen_t n1, n2;
    SEXP x1, x2;
    const char *s1, *s2;
    int eq, cmp, nprot = 0;

    PROTECT(x1 = STRING_ELT(x_, i1)); nprot++;
    PROTECT(x2 = STRING_ELT(x_, i2)); nprot++;

    if (x1 == NA_STRING) {
        eq = (x2 == NA_STRING);
    } else if (x2 == NA_STRING) {
        eq = 0;
    } else {
        n1 = XLENGTH(x1);
        n2 = XLENGTH(x2);
        if (n1 != n2) {
            eq = 0;
        } else {
            s1 = CHAR(x1);
            s2 = CHAR(x2);
            cmp = memcmp(s1, s2, n1);
            eq = (cmp == 0);
        }
    }

    UNPROTECT(nprot);
    return eq;
}
