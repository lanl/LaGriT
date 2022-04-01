#ifndef __LG_INTERFACE_H__
#define __LG_INTERFACE_H__

#include <fc_mangle.h>

typedef int int_ptrsize;
typedef double real8;

#ifdef __cplusplus
extern "C" {
#endif

extern void INSIDE_TET(
        real8 *x1, real8 *y1, real8 *z1,
        real8 *x2, real8 *y2, real8 *z2,
        real8 *x3, real8 *y3, real8 *z3,
        real8 *x4, real8 *y4, real8 *z4,
        real8 *xa, real8 *ya, real8 *za,
        int_ptrsize *flag);

extern void MYTEST();

#ifdef __cplusplus
}
#endif

//extern void lineseg_tri(
//       double *x1, double *y1, double *z1,
//       double *x2, double *y2, double *z2,
//       double *x3, double *y3, double *z3,
//       double *xa, double *ya, double *za,
//       double *xb, double *yb, double *zb,
//       double *x, double *y, double *z,
//       int_ptrsize *flag);

#endif