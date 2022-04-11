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

//////////////////////////////////////////////////////////////////////
/// Fortran interface defined in bind_c_to_fortran.f90
//////////////////////////////////////////////////////////////////////
extern void LG_INTERFACE_CMO_GET_INTINFO_C(
    const char* ioption,   /// [in]
    const char* cmo_name,  /// [in]
    int_ptrsize* iout,     /// [out]
    int_ptrsize* ierr,     /// [out]
    size_t ioption_len,    /// [in]
    size_t cmo_name_len);  /// [in]

// Macro to simplify the above function call
#define CMO_GET_INTINFO LG_INTERFACE_CMO_GET_INTINFO_C

extern void LG_INTERFACE_CMO_GET_STDPTRS_C(
    const char* cmo,
    int* ipimt1,
    int* ipitp1,
    int* ipicr1,
    int* ipisn1,
    double* ipxic,
    double* ipyic,
    double* ipzic,
    int* ipitetclr,
    int* ipitettyp,
    int* ipitetoff,
    int* ipjtetoff,
    int* ipitet,
    int* ipjtet,
    int* ierr,
    size_t cmo_len);

// Macro to simplify the above function call
#define CMO_GET_STDPTRS LG_INTERFACE_CMO_GET_STDPTRS_C

//////////////////////////////////////////////////////////////////////
/// Native LaGriT subroutines
//////////////////////////////////////////////////////////////////////
extern void INITLAGRIT(
    const char* mode,       /// [in]
    const char* log_file,   /// [in]
    const char* batch_file, /// [in]
    size_t mode_len,        /// [in]
    size_t log_file_len,    /// [in]
    size_t batch_file_len); /// [in]

extern void DOTASK(
    const char* cmd,       /// [in]
    int_ptrsize* ierr,     /// [out]
    size_t cmd_len);       /// [in]

extern void CMO_GET_NAME(
    char* name,            /// [out]
    int_ptrsize* ierr,     /// [out]
    size_t name_len);      /// [in]


//extern void CMO_GET_INTINFO_C(
//    const char* ioption,   /// [in]
//    const char* cmo_name,  /// [in]
//    int_ptrsize* iout,     /// [out]
//    int_ptrsize* lout,     /// [out]
//    int_ptrsize* itype,    /// [out]
//    int_ptrsize* ierr,     /// [out]
//    size_t ioption_len,    /// [in]
//    size_t cmo_name_len);  /// [in]

extern void CMO_GET_INFO(
    const char* ioption,
    const char* cmo_name,
    void* ipout,
    int_ptrsize* lout,
    int_ptrsize* itype,
    int_ptrsize* ierr,
    size_t ioption_len,
    size_t cmo_name_len);

extern void CMO_GET_ATTINFO(
    const char*     ioption,       // [in]
    const char*     cmo_name,      // [in]
    int_ptrsize*    iout,          // [out]
    double*         rout,          // [out]
    char*           cout,          // [out]
    double**        ipout,         // [out]
    int_ptrsize*    lout,          // [out]
    int_ptrsize*    itype,         // [out]
    int_ptrsize*    ierr,          // [out]
    size_t          ioption_len,   // [in]
    size_t          cmo_name_len); // [in]

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