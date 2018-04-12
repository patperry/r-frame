#include "rframe.h"
#include <math.h>
#include <string.h>


static void rframe_subscript_double_error(R_xlen_t n, double index)
{
    double max;

    if (isnan(index)) {
        Rf_error("numeric subscript contains an NA value");
    } else if (index < 0) {
        Rf_error("numeric subscript contains both positive"
                 " and negative values");
    } else if (isinf(index)) {
        Rf_error("bounds error: index is Inf, maximum is %.0f",
                (double)n);
    } else {
        max = (index <= R_XLEN_T_MAX) ? (double)n : R_XLEN_T_MAX;
        Rf_error("bounds error: index is %.0f, maximum is %.0f",
                index, max);
    }
}


static SEXP rframe_subscript_double_negative(SEXP value_, R_xlen_t n)
{
    SEXP ans_;
    const double *value;
    double index, max, *ans;
    int *discard;
    R_xlen_t i, j, ni, ndiscard;
    
    value = REAL(value_);
    ni = XLENGTH(value_);
    max = (double)n + 1;

    ndiscard = 0;
    discard = (void *)R_alloc(n, sizeof(*discard));
    memset(discard, 0, n * sizeof(*discard));

    for (i = 0; i < ni; i++) {
        index = value[i];
        if (isnan(index)) {
            rframe_subscript_double_error(n, index);
        }
        index = -index;
        if (index < 0) {
            rframe_subscript_double_error(n, index);
        }
        if (1 <= index && index < max) {
            j = (R_xlen_t)(index - 1);
            if (!discard[j]) {
                discard[j] = 1;
                ndiscard++;
            }
        }
    }

    PROTECT(ans_ = Rf_allocVector(REALSXP, n - ndiscard));
    ans = REAL(ans_);

    for (i = 0; i < n; i++) {
        if (!discard[i]) {
            *ans++ = (double)(i + 1);
        }
    }

    UNPROTECT(1);
    return ans_;
}


static SEXP rframe_subscript_double(SEXP value_, R_xlen_t n, int get)
{
    SEXP ans_, names_, vnames_;
    const double *value;
    double *ans;
    double index, max;
    R_xlen_t i, j, ni, nz;
    int nprot = 0;

    value = REAL(value_);
    ni = XLENGTH(value_);
    max = get ? ((double)n + 1) : R_XLEN_T_MAX;
    nz = 0;

    for (i = 0; i < ni; i++) {
        index = value[i];
        if (1 <= index) {
            if (index >= max) {
                rframe_subscript_double_error(n, index);
            }
        } else if (0 <= index) {
            nz++;
        } else if (isnan(index)) {
            rframe_subscript_double_error(n, index);
        } else {
            rframe_subscript_double_negative(value_, n);
        }
    }

    if (nz == 0) {
        return value_;
    }

    PROTECT(ans_ = Rf_allocVector(REALSXP, ni - nz)); nprot++;
    ans = REAL(ans_);

    for (i = 0; i < ni; i++) {
        index = value[i];
        if (index >= 1) {
            *ans++ = index;
        }
    }

    vnames_ = Rf_getAttrib(value_, R_NamesSymbol);
    if (vnames_ != R_NilValue) {
        PROTECT(vnames_); nprot++;
        PROTECT(names_ = Rf_allocVector(STRSXP, ni - nz)); nprot++;
        j = 0;
        for (i = 0; i < ni; i++) {
            index = value[i];
            if (index >= 0) {
                SET_STRING_ELT(names_, j, STRING_ELT(vnames_, i));
                j++;
            }
        }
        Rf_setAttrib(ans_, R_NamesSymbol, names_);
    }

    UNPROTECT(nprot);
    return ans_;
}


static SEXP rframe_subscript_logical(SEXP value_, R_xlen_t n, int get)
{
    SEXP ans_, names_, vnames_;
    double *ans;
    R_xlen_t i, j, nvalue, nz;
    const int *value;
    int nprot = 0;

    value = LOGICAL(value_);
    nvalue = XLENGTH(value_);

    if (nvalue != n) {
        Rf_error("mismatch: logical subscript length is %.0f, should be %.0f",
                (double)nvalue, (double)n);
    }

    nz = 0;
    for (i = 0; i < n; i++) {
        if (value[i] == TRUE) {
            nz++;
        }
    }

    PROTECT(ans_ = Rf_allocVector(REALSXP, nz)); nprot++;
    ans = REAL(ans_);

    for (i = 0; i < n; i++) {
        if (value[i] == TRUE) {
            *ans++ = (double)(i + 1);
        }
    }

    if (get) {
        vnames_ = Rf_getAttrib(value_, R_NamesSymbol);
        if (vnames_ != R_NilValue) {
            PROTECT(vnames_); nprot++;
            PROTECT(names_ = Rf_allocVector(STRSXP, nz)); nprot++;
            j = 0;
            for (i = 0; i < n; i++) {
                if (value[i] ==TRUE) {
                    SET_STRING_ELT(names_, j, STRING_ELT(vnames_, i));
                    j++;
                }
            }
            Rf_setAttrib(ans_, R_NamesSymbol, names_);
        }
    }

    UNPROTECT(nprot);
    return ans_;
}


static SEXP rframe_subscript_character(SEXP value_, R_xlen_t n,
                                       SEXP names_, int get)
{
    (void)value_;
    (void)n;
    (void)names_;
    (void)get;
    return R_NilValue;
}


SEXP rframe_subscript(SEXP value_, SEXP n_, SEXP names_, SEXP get_)
{
    SEXP ans_;
    R_xlen_t n;
    int get, nprot = 0;

    if (TYPEOF(n_) == REALSXP) {
        n = (R_xlen_t)REAL(n_)[0];
    } else {
        n = (R_xlen_t)INTEGER(n_)[0];
    }

    get = LOGICAL(get_)[0] == TRUE;

    switch (TYPEOF(value_)) {
    case INTSXP:
        PROTECT(value_ = Rf_coerceVector(value_, REALSXP)); nprot++;
        // fallthrough

    case REALSXP:
        ans_ = rframe_subscript_double(value_, n, get);
        break;

    case LGLSXP:
        ans_ = rframe_subscript_logical(value_, n, get);
        break;

    case STRSXP:
        ans_ = rframe_subscript_character(value_, n, names_, get);
        break;

    default:
        Rf_error("invalid subscript type");
    }

    UNPROTECT(nprot);
    return ans_;
}
