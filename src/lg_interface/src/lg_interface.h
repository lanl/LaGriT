/*
 * FORTRAN - C BINDING
 *
 * This header file is used for exposing
 * Fortran subroutines to C and C++ code.
 * 
 * Any Fortran function/subroutine that
 * you wish to expose:
 *   - PRIVATELY:
 *       - Declare Fortran subroutines
 *         that *internal* C/C++ code
 *         reference
 *         (i.e. anothermatbl3d.cpp)
 * 
 *   - PUBLICLY:
 *        - Declare Fortran subroutines
 *          that you wish to expose to
 *          end-users of a LaGriT shared
 *          library.
 *        - Actual public declarations
 *          are in `lagrit.h`.
 *        - See also: `public_interface.cpp`
 * 
 * NOTE: This is *not* a public header.
 */

#ifndef __LG_INTERFACE_H__
#define __LG_INTERFACE_H__

#include <fc_mangle.h>
#include <stddef.h>

typedef int int_ptrsize;
typedef double real8;

#ifdef __cplusplus
extern "C" {
#endif

extern void INITLAGRIT(
    const char* mode,
    const char* log_file,
    const char* batch_file,
    size_t mode_len,
    size_t log_file_len,
    size_t batch_file_len);

extern void DOTASK(
    const char* cmd,
    int_ptrsize* ierr,
    size_t cmd_len);

extern void CMO_GET_NAME(
    const char* name,
    int_ptrsize* ierr,
    size_t name_len);

extern void CMO_GET_INTINFO(
    const char* ioption,
    const char* cmo_name,
    int_ptrsize* iout,
    int_ptrsize* iout_len,
    int_ptrsize* itype,
    int_ptrsize* ierr,
    size_t ioption_len,
    size_t cmo_name_len);

extern void CMO_GET_INFO(
    const char* ioption,
    const char* cmo_name,
    void* ipout,
    int_ptrsize* lout,
    int_ptrsize* itype,
    int_ptrsize* ierr,
    size_t ioption_len,
    size_t cmo_name_len);

extern void INSIDE_TET(
    real8 *x1, real8 *y1, real8 *z1,
    real8 *x2, real8 *y2, real8 *z2,
    real8 *x3, real8 *y3, real8 *z3,
    real8 *x4, real8 *y4, real8 *z4,
    real8 *xa, real8 *ya, real8 *za,
    int_ptrsize *flag);

extern void LINESEG_TRI(
    double *x1, double *y1, double *z1,
    double *x2, double *y2, double *z2,
    double *x3, double *y3, double *z3,
    double *xa, double *ya, double *za,
    double *xb, double *yb, double *zb,
    double *x, double *y, double *z,
    int_ptrsize *flag);

#ifdef __cplusplus
}
#endif

#endif