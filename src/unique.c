#include "rframe.h"
#include <string.h>

// Initial array size. Must be positive.
#define ARRAY_SIZE_INIT 32

// Maximum occupy percentage before we resize. Must be in (0, 1].
#define TABLE_LOAD_FACTOR 0.75

// Initial table size. Must be a positive power of 2.
#define TABLE_SIZE_INIT 32

// Code for an empty table item
#define TABLE_ITEM_EMPTY (R_xlen_t)(-1)

typedef struct {
    R_xlen_t *items;
    R_xlen_t count;
    R_xlen_t size;
} Array;


void array_init(Array *a, R_xlen_t size)
{
    if (size <= ARRAY_SIZE_INIT) {
        size = ARRAY_SIZE_INIT;
    }

    a->size = size;
    a->count = 0;
    a->items = (void *)R_alloc(a->size, sizeof(*a->items));
}


void array_grow(Array *a)
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


void array_push(Array *a, R_xlen_t item)
{
    R_xlen_t nmax = a->size;
    R_xlen_t n = a->count;

    if (n == nmax) {
        array_grow(a);
    }

    a->items[n] = item;
    a->count = n + 1;
}


typedef struct {
    R_xlen_t *items;
    R_xlen_t size;
    R_xlen_t capacity;
    uint64_t mask;
} Table;


typedef struct {
    uint64_t mask;
    uint64_t hash;
    uint64_t nprobe;
    uint64_t index;
} Probe;


// Start a new table probe at the given hash code.
void table_probe_make(Table *t, Probe *p, uint64_t hash)
{
    p->mask = t->mask;
    p->hash = hash;
    p->nprobe = 0;
}

// Advance a probe to the next index in the sequence. Calling this function
// repeatedly will loop over all indices in the hash table.
int table_probe_advance(Probe *p)
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


void table_init(Table *t, R_xlen_t size)
{
    R_xlen_t i;

    if (size <= TABLE_SIZE_INIT) {
        size = TABLE_SIZE_INIT;
    }

    t->size = size;
    t->mask = (uint64_t)(t->size - 1);
    t->capacity = (R_xlen_t)(TABLE_LOAD_FACTOR * t->size);
    t->items = (void *)R_alloc(t->size, sizeof(*t->items));

    for (i = 0; i < t->size; i++) {
        t->items[i] = TABLE_ITEM_EMPTY;
    }
}


void table_grow(Table *t, const Array *types, const uint64_t *hash)
{
    Table t2;
    Probe p;
    R_xlen_t i, j, m;

    table_init(&t2, 2 * t->size);

    m = types->count;
    for (j = 0; j < m; j++) {
        RFRAME_CHECK_INTERRUPT(j);

        i = types->items[j];

        table_probe_make(&t2, &p, hash[i]);
        while (table_probe_advance(&p)) {
            if (t2.items[p.index] == TABLE_ITEM_EMPTY) {
                t2.items[p.index] = j;
                break;
            }
        }
    }

    *t = t2;
}


SEXP rframe_unique(SEXP x_)
{
    SEXP id_, table_, typehash_, typerows_, names_, out_;
    Array types;
    Table t;
    Probe p;
    R_xlen_t i, j, n;
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

    array_init(&types, 0);
    table_init(&t, 0);

    for (i = 0; i < n; i++) {
        RFRAME_CHECK_INTERRUPT(i);

        table_probe_make(&t, &p, hash[i]);

        while (table_probe_advance(&p)) {
            j = t.items[p.index];

            if (j == TABLE_ITEM_EMPTY) {

                j = types.count;
                array_push(&types, i);
                t.items[p.index] = j;

                if (types.count == t.capacity) {
                    table_grow(&t, &types, hash);
                }

                break;
            } else if (hash[i] != hash[types.items[j]]) {
                continue;
            } else if (rframe_equals_dataset(x_, i, types.items[j])) {
                break;
            }
        }

        id[i] = (double)(j + 1);
    }

    PROTECT(table_ = Rf_allocVector(REALSXP, t.size)); nprot++;
    table = REAL(table_);
    for (i = 0; i < t.size; i++) {
        j = t.items[i];
        if (j == TABLE_ITEM_EMPTY) {
            table[i] = 0;
        } else {
            table[i] = (double)(j + 1);
        }
    }

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