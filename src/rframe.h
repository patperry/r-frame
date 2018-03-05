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

R_xlen_t rframe_nrow_dataset(SEXP x_);

SEXP rframe_groups(SEXP x_, SEXP sort_);

int rframe_equals_dataset(SEXP x_, R_xlen_t i1, R_xlen_t i2);
void rframe_hash_init(uint64_t *hash, R_xlen_t n);
void rframe_hash_dataset(uint64_t *hash, R_xlen_t n, SEXP x_);
void rframe_hash_final(uint64_t *hash, R_xlen_t n);

#endif /* RFRAME_H */
