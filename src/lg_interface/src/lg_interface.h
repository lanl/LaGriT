#ifndef __LG_INTERFACE_H__
#define __LG_INTERFACE_H__

#include <fc_mangle.h>

typedef int int_ptrsize;

extern void inside_tet(
        double *x1, double *y1, double *z1,
        double *x2, double *y2, double *z2,
        double *x3, double *y3, double *z3,
        double *x4, double *y4, double *z4,
        double *xa, double *ya, double *za,
        int_ptrsize *flag);

extern void lineseg_tri(
        double *x1, double *y1, double *z1,
        double *x2, double *y2, double *z2,
        double *x3, double *y3, double *z3,
        double *xa, double *ya, double *za,
        double *xb, double *yb, double *zb,
        double *x, double *y, double *z,
        int_ptrsize *flag);

#endif