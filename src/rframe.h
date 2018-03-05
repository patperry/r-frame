#ifndef RFRAME_H
#define RFRAME_H

#define R_NO_REMAP
#include <Rdefines.h>

#define RFRAME_CHECK_EVERY 1000
#define RFRAME_CHECK_INTERRUPT(i) \
        do { \
                if (((i) + 1) % RFRAME_CHECK_EVERY == 0) { \
                        R_CheckUserInterrupt(); \
                } \
        } while (0)

SEXP rframe_groups(SEXP x_, SEXP sort_);

#endif /* RFRAME_H */
