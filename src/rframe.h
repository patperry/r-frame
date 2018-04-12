#ifndef RFRAME_H
#define RFRAME_H

#include <stdint.h>

#define R_NO_REMAP
#include <Rdefines.h>

#define RFRAME_CHECK_EVERY 1000
#define RFRAME_CHECK_INTERRUPT(i) \
        do { \
                if (((i) + 1) % RFRAME_CHECK_EVERY == 0) { \
                        R_CheckUserInterrupt(); \
                } \
        } while (0)

SEXP rframe_as_simple_double(SEXP x_);
SEXP rframe_split_group(SEXP group_, SEXP ngroup_);
SEXP rframe_subset(SEXP x_, SEXP i_);
SEXP rframe_subscript(SEXP value_, SEXP n_, SEXP names_, SEXP get_);

R_xlen_t rframe_nrow_dataset(SEXP x_);
SEXP rframe_rowid_keyset(SEXP keyset_, SEXP x_, SEXP def_);
SEXP rframe_unique(SEXP x_);

int rframe_equals_dataset(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2);
int rframe_equals_character(SEXP x1_, R_xlen_t i1, SEXP x2_, R_xlen_t i2);

void rframe_hash_init(uint64_t *hash, R_xlen_t n);
void rframe_hash_dataset(uint64_t *hash, R_xlen_t n, SEXP x_);
void rframe_hash_final(uint64_t *hash, R_xlen_t n);


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
static inline void rframe_probe_init(struct rframe_probe *p,
                                     const struct rframe_table *t,
                                     uint64_t hash)
{
    p->mask = t->mask;
    p->hash = hash;
    p->nprobe = 0;
}

// Advance a probe to the next index in the sequence. Calling this function
// repeatedly will loop over all indices in the hash table.
static inline int rframe_probe_advance(struct rframe_probe *p)
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


#endif /* RFRAME_H */
