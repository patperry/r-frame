#include "rframe.h"
#include <string.h>

// Initial array size. Must be positive.
#define RFRAME_ARRAY_SIZE_INIT 32

// Maximum occupy percentage before we resize. Must be in (0, 1].
#define RFRAME_TABLE_LOAD_FACTOR 0.75

// Initial table size. Must be a positive power of 2.
#define RFRAME_TABLE_SIZE_INIT 32


struct rframe_array {
    R_xlen_t *items;
    R_xlen_t count;
    R_xlen_t size;
};


void rframe_array_init(struct rframe_array *a, R_xlen_t size)
{
    if (size <= RFRAME_ARRAY_SIZE_INIT) {
        size = RFRAME_ARRAY_SIZE_INIT;
    }

    a->size = size;
    a->count = 0;
    a->items = (void *)R_alloc(a->size, sizeof(*a->items));
}


void rframe_array_grow(struct rframe_array *a)
{
    R_xlen_t n0, n1;
    R_xlen_t *items0, *items1;

    n0 = a->size;
    n1 = 2 * n0;

    items0 = a->items;
    items1 = (void *)R_alloc(n1, sizeof(*items1));
    memcpy(items1, items0, n0 * sizeof(*items0));

    a->items = items1;
    a->size = n1;
}


void rframe_array_push(struct rframe_array *a, R_xlen_t item)
{
    R_xlen_t nmax = a->size;
    R_xlen_t n = a->count;

    if (n == nmax) {
        rframe_array_grow(a);
    }

    a->items[n] = item;
    a->count = n + 1;
}


struct rframe_table {
    double *items;
    R_xlen_t size;
    R_xlen_t capacity;
    uint64_t mask;
};


struct rframe_probe {
    uint64_t mask;
    uint64_t hash;
    uint64_t nprobe;
    uint64_t index;
};


// Start a new table probe at the given hash code.
void rframe_probe_init(struct rframe_probe *p,
                       const struct rframe_table *t,
                       uint64_t hash)
{
    p->mask = t->mask;
    p->hash = hash;
    p->nprobe = 0;
}

// Advance a probe to the next index in the sequence. Calling this function
// repeatedly will loop over all indices in the hash table.
int rframe_probe_advance(struct rframe_probe *p)
{
    uint64_t index;

    if (p->nprobe == 0) {
        index = p->hash;
    } else {
        // Quadratic probing:
        //
        //     h(k,i) = h(k) + 0.5 i + 0.5 i^2
        //
        // index += nprobe;
        //
        // When the table size m is a power of 2, the values h(k,i) % m
        // are all distinct for i = 0, 1, ..., m - 1
        //
        // https://en.wikipedia.org/wiki/Quadratic_probing
        index = p->index + p->nprobe;
    }

    index &= p->mask;
    p->nprobe++;
    p->index = index;
    return 1;
}


void rframe_table_init(struct rframe_table *t, R_xlen_t size)
{
    if (size <= RFRAME_TABLE_SIZE_INIT) {
        size = RFRAME_TABLE_SIZE_INIT;
    }

    t->size = size;
    t->mask = (uint64_t)(t->size - 1);
    t->capacity = (R_xlen_t)(RFRAME_TABLE_LOAD_FACTOR * t->size);
    t->items = (void *)R_alloc(t->size, sizeof(*t->items));
    memset(t->items, 0, t->size * sizeof(*t->items));
}


void rframe_table_grow(struct rframe_table *t,
                       const struct rframe_array *types,
                       const uint64_t *hash)
{
    struct rframe_table t2;
    struct rframe_probe p;
    R_xlen_t i, j, m;

    rframe_table_init(&t2, 2 * t->size);

    m = types->count;
    for (j = 0; j < m; j++) {
        RFRAME_CHECK_INTERRUPT(j);

        i = types->items[j];

        rframe_probe_init(&p, &t2, hash[i]);
        while (rframe_probe_advance(&p)) {
            if (t2.items[p.index] == 0) {
                t2.items[p.index] = (double)(j + 1);
                break;
            }
        }
    }

    *t = t2;
}


SEXP rframe_unique(SEXP x_)
{
    SEXP id_, table_, typehash_, typerows_, names_, out_;
    struct rframe_array types;
    struct rframe_table t;
    struct rframe_probe p;
    R_xlen_t i, j, n, item;
    uint64_t *hash;
    double *id, *table, *typehash, *typerows;
    int nprot = 0;
   
    n = rframe_nrow_dataset(x_);
    PROTECT(id_ = Rf_allocVector(REALSXP, n)); nprot++;
    id = REAL(id_);

    hash = (void *)R_alloc(n, sizeof(*hash));
    rframe_hash_init(hash, n);
    rframe_hash_dataset(hash, n, x_);
    rframe_hash_final(hash, n);

    rframe_array_init(&types, 0);
    rframe_table_init(&t, 0);

    for (i = 0; i < n; i++) {
        RFRAME_CHECK_INTERRUPT(i);

        rframe_probe_init(&p, &t, hash[i]);

        while (rframe_probe_advance(&p)) {
            item = (R_xlen_t)t.items[p.index];

            if (item == 0) {

                item = types.count + 1;
                rframe_array_push(&types, i);
                t.items[p.index] = (double)item;

                if (types.count == t.capacity) {
                    rframe_table_grow(&t, &types, hash);
                }

                break;
            } else {
                j = item - 1;
                if (hash[i] != hash[types.items[j]]) {
                    continue;
                } else if (rframe_equals_dataset(x_, i, types.items[j])) {
                    break;
                }
            }
        }

        id[i] = (double)item;
    }

    PROTECT(table_ = Rf_allocVector(REALSXP, t.size)); nprot++;
    table = REAL(table_);
    memcpy(table, t.items, t.size * sizeof(*table));

    PROTECT(typerows_ = Rf_allocVector(REALSXP, types.count)); nprot++;
    PROTECT(typehash_ = Rf_allocVector(REALSXP, types.count)); nprot++;
    typerows = REAL(typerows_);
    typehash = REAL(typehash_);
    for (j = 0; j < types.count; j++) {
        typerows[j] = (double)(types.items[j] + 1);
        typehash[j] = (double)hash[types.items[j]];
    }

    PROTECT(names_ = Rf_allocVector(STRSXP, 4)); nprot++;
    SET_STRING_ELT(names_, 0, Rf_mkChar("group"));
    SET_STRING_ELT(names_, 1, Rf_mkChar("types"));
    SET_STRING_ELT(names_, 2, Rf_mkChar("hash"));
    SET_STRING_ELT(names_, 3, Rf_mkChar("table"));

    PROTECT(out_ = Rf_allocVector(VECSXP, 4)); nprot++;
    SET_VECTOR_ELT(out_, 0, id_);
    SET_VECTOR_ELT(out_, 1, typerows_);
    SET_VECTOR_ELT(out_, 2, typehash_);
    SET_VECTOR_ELT(out_, 3, table_);
    Rf_setAttrib(out_, R_NamesSymbol, names_);

    UNPROTECT(nprot);
    return out_;
}
