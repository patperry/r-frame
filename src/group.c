#include "rframe.h"


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
