#include "rframe.h"


SEXP rframe_rowid_keyset(SEXP keyset_, SEXP x_, SEXP def_)
{
    SEXP keyhash_, keytable_, id_;
    struct rframe_table t;
    struct rframe_probe p;
    const double *keyhash;
    R_xlen_t i, j, n;
    double *id, item;
    uint64_t *hash;
    double def;
    int nprot = 0;

    keyhash_ = Rf_getAttrib(keyset_, Rf_install("keyset.hash"));
    keyhash = REAL(keyhash_);

    keytable_ = Rf_getAttrib(keyset_, Rf_install("keyset.table"));
    t.items = REAL(keytable_);
    t.capacity = XLENGTH(keytable_);
    t.mask = (uint64_t)t.capacity - 1;
    t.size = XLENGTH(keyhash_);

    n = rframe_nrow_dataset(x_);
    def = REAL(def_)[0];

    PROTECT(id_ = Rf_allocVector(REALSXP, n)); nprot++;
    id = REAL(id_);
    
    hash = (void *)R_alloc(n, sizeof(*hash));
    rframe_hash_init(hash, n);
    rframe_hash_dataset(hash, n, x_);
    rframe_hash_final(hash, n);
    
    for (i = 0; i < n; i++) {
        RFRAME_CHECK_INTERRUPT(i);

        rframe_probe_init(&p, &t, hash[i]);

        while (rframe_probe_advance(&p)) {
            item = t.items[p.index];

            if (item == 0) {
                item = def;
                break;
            } else {
                j = (R_xlen_t)item - 1;
                if (hash[i] != (uint64_t)keyhash[j]) {
                    continue;
                } else if (rframe_equals_dataset(x_, i, keyset_, j)) {
                    break;
                }
            }
        }

        id[i] = item;
    }

    UNPROTECT(nprot);
    return id_;
}
