#pragma once

#include <lagrit_cmake_interface.h>

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


//void lg_inside_tet(
//        double *x1, double *y1, double *z1,
//        double *x2, double *y2, double *z2,
//        double *x3, double *y3, double *z3,
//        double *x4, double *y4, double *z4,
//        double *xa, double *ya, double *za,
//        int_ptrsize *flag);

//#define inside_tet_ inside_tet

//void lg_lineseg_tri(
//        double *x1, double *y1, double *z1,
//        double *x2, double *y2, double *z2,
//        double *x3, double *y3, double *z3,
//        double *xa, double *ya, double *za,
//        double *xb, double *yb, double *zb,
//        double *x, double *y, double *z,
//        int_ptrsize *flag);

//#define lineseg_tri_ lg_lineseg_tri