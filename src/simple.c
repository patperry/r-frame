#include "rframe.h"

SEXP rframe_as_simple_double(SEXP x_)
{
    R_xlen_t i, n;
    double *x;
    int dup;

    dup = 0;
    n = XLENGTH(x_);
    x = REAL(x_);

    for (i = 0; i < n; i++) {
        if (x[i] == 0) {
            // replace -0 with 0
            if (copysign(1, x[i]) < 0) {
                if (!dup) {
                    PROTECT(x_ = Rf_duplicate(x_));
                    x = REAL(x_);
                    dup = 1;
                }
                x[i] = 0;
            }
        } else if (R_IsNaN(x[i])) {
            // replace non-canonical NaN with NaN (leave NA as-is)

            // the current implementation is conservative, copies on *any* NaN;
            // copying is only necessary when NaN is other than canonical NaN
            if (!dup) {
                PROTECT(x_ = Rf_duplicate(x_));
                x = REAL(x_);
                dup = 1;
            }
            x[i] = R_NaN;
        }
    }

    if (dup) {
        UNPROTECT(1);
    }

    return x_;
}
