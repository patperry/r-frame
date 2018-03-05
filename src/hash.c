#include "rframe.h"
#include <assert.h>


static void rframe_hash_column(uint64_t *hash, R_xlen_t n, SEXP x_);
static void rframe_hash_logical(uint64_t *hash, R_xlen_t n, SEXP x_);
static void rframe_hash_integer(uint64_t *hash, R_xlen_t n, SEXP x_);
static void rframe_hash_double(uint64_t *hash, R_xlen_t n, SEXP x_);
static void rframe_hash_character(uint64_t *hash, R_xlen_t n, SEXP x_);

static uint64_t rframe_hash_combine(uint64_t seed, uint64_t hash);
static uint64_t rframe_hash_string(const char *str);


void rframe_hash_dataset(uint64_t *hash, R_xlen_t n, SEXP x_)
{
    SEXP col;
    R_xlen_t j, ncol;

    ncol = XLENGTH(x_);
    for (j = 0; j < ncol; j++) {
        col = VECTOR_ELT(x_, j);
        rframe_hash_column(hash, n, col);
    }
}


void rframe_hash_column(uint64_t *hash, R_xlen_t n, SEXP x_)
{
    int t = TYPEOF(x_);

    switch (t) {
    case LGLSXP:
        rframe_hash_logical(hash, n, x_);
        break;

    case INTSXP:
        rframe_hash_integer(hash, n, x_);
        break;

    case REALSXP:
        rframe_hash_double(hash, n, x_);
        break;

    case STRSXP:
        rframe_hash_character(hash, n, x_);
        break;

    case LISTSXP:
        rframe_hash_dataset(hash, n, x_);
        break;

    default:
        assert(t == NILSXP);
        break;
    }
}


void rframe_hash_logical(uint64_t *hash, R_xlen_t n, SEXP x_)
{
    const int *x;
    R_xlen_t i;
    uint64_t h;

    x = LOGICAL(x_);
    for (i = 0; i < n; i++) {
        RFRAME_CHECK_INTERRUPT(i);

        h = (uint64_t)x[i];
        hash[i] = rframe_hash_combine(hash[i], h);
    }
}


void rframe_hash_integer(uint64_t *hash, R_xlen_t n, SEXP x_)
{
    const int *x;
    R_xlen_t i;
    uint64_t h;

    x = INTEGER(x_);
    for (i = 0; i < n; i++) {
        RFRAME_CHECK_INTERRUPT(i);
        
        h = (uint64_t)x[i];
        hash[i] = rframe_hash_combine(hash[i], h);
    }
}


void rframe_hash_double(uint64_t *hash, R_xlen_t n, SEXP x_)
{
    const double *x;
    R_xlen_t i;
    union {
        double d;
        uint64_t u;
    } value;

    x = REAL(x_);
    for (i = 0; i < n; i++) {
        RFRAME_CHECK_INTERRUPT(i);
        
        value.d = x[i];
        hash[i] = rframe_hash_combine(hash[i], value.u);
    }
}


void rframe_hash_character(uint64_t *hash, R_xlen_t n, SEXP x_)
{
    SEXP x;
    R_xlen_t i;
    uint64_t h;

    for (i = 0; i < n; i++) {
        RFRAME_CHECK_INTERRUPT(i);
        
        x = STRING_ELT(x_, i);
        if (x == NA_STRING) {
            h = (uint64_t)-1;
        } else {
            h = rframe_hash_string(CHAR(x));
        }
        hash[i] = rframe_hash_combine(hash[i], h);
    }
}


void rframe_hash_init(uint64_t *hash, R_xlen_t n)
{
    memset(hash, 0, n * sizeof(*hash));
}


// This is the hash combine function used by the Boost library
// (boost/functional/hash/hash.hpp).  It is a variant of the function employed
// by Hoad and Zobel. Those authors in turn cite Ramakrishna and Zobel.
//
// References:
//
// Hoad, T. C., & Zobel, J. (2003). Methods for identifying versioned and
// plagiarized documents. Journal of the Association for Information Science
// and Technology, 54(3), 203-215.
//
// Ramakrishna, M. V., & Zobel, J. (1997). Performance in practice of string
// hashing functions. In Database Systems For Advanced Applications' 97 (pp.
// 215-223).
//
uint64_t rframe_hash_combine(uint64_t seed, uint64_t hash)
{
    seed ^= hash + 0x9e3779b9 + (seed << 6) + (seed >> 2);
    return seed;
}


// Dan Bernstein's djb2 XOR hash: http://www.cse.yorku.ca/~oz/hash.html
uint64_t rframe_hash_string(const char *str)
{
    uint64_t hash = 5381;
    int ch;

    while ((ch = *str++)) {
        hash = ((hash << 5) + hash) + (uint64_t)ch; /* hash * 33 + c */
    }

    return hash;
}



