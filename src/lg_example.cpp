/* C++ examples using wrappers to fortran calls 

LaGriT fortran subroutines called with wrappers
lg_fc_wrappers.f90 - used to assign mesh object cray pointer to c pointer
lg_c_wrappers.cpp - cpp wrappers for dotask and cmo_get_info routines
                     - Daniel's public_interface.cpp with some removed
lg_f_interface.h  - fortran declarations for routines in public_interface.cpp
lg_c_interface.h  - constants and c declarations
fc_mangle.h     - created by cmake and CMakeLists.tx to mangle fortran declarations

In general: 
lg_routines are C++ wrappers calling fortran using C arguments
fc_routines are f90 wrappers calling fortran using c-fortran arguments
In all cases types passed must match types in fortran
where integer=8bytes, real*8=8bytes, pointer=8bytes

*/

#include "lg_c_interface.h"
#include "lg_f_interface.h"
#include "type_sizes.h"
#include <stdio.h>
#include <cstring>

#ifdef __cplusplus
extern "C" {
#endif

void example_cpp_ () {

    LG_ERR err = 0;
    double minmax[6];
    double xreal = 0.0;
    long  ival= 0;
    long  itype = 0;
    long  ierr= 0;

    printf("\n===== Begin C++ wrapper examples  =========\n\n");

/* ----------------------------------------------------------
C     Create a mesh object using dotask wrappers 
C     This will be used for the cmo_get calls 
C ----------------------------------------------------------*/

    printf("------------------------------------------\n");
    printf("Use DOTASK to create mesh object\n");

//  Delete possible mesh object first
//  Create hex mesh named tmp_hex
//  Set node imt1 to 1 and set boundary tags in itp1

    char* cmd1[512];

    err = lg_dotask("cmo/delete/tmp_hex");

    const char* cmds[] = {
        "cmo/create/tmp_hex/ / /hex",
        "createpts/brick/xyz/3,3,3 /1.,2.,3./1.5,2.5,3.5/"};

//  Check result
    for (int i = 0; i < sizeof(cmds)/sizeof(cmds[0]); ++i) {
        err = lg_dotask(cmds[i]);
        if (err != LG_ERR_SUCCESS) {
            printf("Command execution failed: %s\n", cmds[i]);
            return;
        }
    }
    err = lg_dotask("cmo/setatt/tmp_hex/imt1/1");
    err = lg_dotask("cmo/set_id/tmp_hex/element/");
    err = lg_dotask("cmo/copyatt/tmp_hex tmp_hex/itetclr id_elem/");
    err = lg_dotask("resetpts/itp");
    err = lg_dotask("cmo/status/tmp_hex/");
    err = lg_dotask("cmo/printatt/tmp_hex/-all-/minmax/");

/* ----------------------------------------------------------
C     Get current mesh name
C ----------------------------------------------------------*/

    printf("------------------------------------------\n");
    printf("\nLG_CMO_GET_NAME\n");

    char cmo_name[32];
    err = lg_cmo_get_name(cmo_name, 32);

    if (err != LG_ERR_SUCCESS) {
        printf("Failed to get cmo name\n");
        return;
    } else {
        printf("current cmo name: %s\n",cmo_name);
    }

/* ----------------------------------------------------------
C     Get mesh integer values using cmo_get_intinfo
C     This works for integer values only
C ----------------------------------------------------------*/

    printf("------------------------------------------\n");
    printf("\nCMO_GET_INTINFO for type INT\n");

    int nnodes = lg_cmo_get_intinfo("nnodes", cmo_name);
    int nelements = lg_cmo_get_intinfo("nelements", cmo_name);
    int ndim = lg_cmo_get_intinfo("ndimensions_geom", cmo_name);

    // Should not evaluate true if any of the above calls failed
    if (err == LG_ERR_SUCCESS) {
        printf("CMO name: '%s'\n", cmo_name);
        printf("return nnodes: %d\n", nnodes);
        printf("return nelements: %d\n", nelements);
        printf("return ndimensions_geom: %d\n", ndim);
    }

/* ----------------------------------------------------------
C Get mesh attributes f90 fortran wrappers 
C   fc_cmo_get_int    fc_cmo_get_vint
C   fc_cmo_get_double fc_cmo_get_vdouble
C   All args passed as pointers except char string
C   Char string must pass lenth at end of args
C   args passed to fortran should be size 8
C ----------------------------------------------------------*/

    double xval = 0;
    long nlen = 0;
    long nelem = 0;
    double *xptr;
    long *iptr;
    long *iptr2;

    long icmolen;
    long iattlen;

    int i = 0;
    int ierror = 0;

    char att1[ ]="imt";
    char att2[ ]="itetclr";
    char att3[ ]="zic";

    printf("------------------------------------------\n");
    printf("Show Sizes being used:\n");

    // IMPORTANT passed sizes must be same as fortran 8 bytes
    printf("Size C xptr: %ld\n",sizeof(xptr) );
    printf("Size C iptr: %ld\n",sizeof(iptr) );
    printf("Size C xval: %ld\n",sizeof(xval) );
    printf("Size C long nlen: %ld\n",sizeof(nlen) );
    printf("Size C int nnodes: %ld\n",sizeof(nnodes) );

    printf("\nGET ATTRIBUTES for CMO:'%s'\n", cmo_name);

    icmolen = strlen(cmo_name);
    iattlen = strlen(att1);
    fc_cmo_get_vint_(cmo_name,att1,&iptr,&nlen,&ierr,icmolen,iattlen);

    iattlen = strlen(att2);
    fc_cmo_get_vint_(cmo_name,att2,&iptr2,&nelem,&ierr,icmolen,iattlen);


    printf("return imt nlength: %d\n", nlen);
    printf("return itetclr nlength: %d\n", nelem);
    ierror = 0;

    if (ierror == 0) { 
      printf("*iptrs = ");
      for( i = 0; i < 5; i = i + 1 ){
        printf(" %3ld, %3ld  ", *(iptr+i),*(iptr2+i));
      }
      printf("\n");
    }

    printf("------------------------------------------\n");
    printf("Get VDOUBLE pointer and length\n\n");

    iattlen = strlen(att3);
    fc_cmo_get_vdouble_(cmo_name,att3,&xptr,&nlen,&ierr,icmolen,iattlen);


    printf("return zic nlength: %d\n", nlen);
    ierror = 0;

    if (ierror == 0) {
      printf("*xptr = ");
      for( i = 0; i < 5; i = i + 1 ){
        printf(" %3f ", *(xptr+i) );
      }
      printf("\n");
    }

    printf("------------------------------------------\n");
    printf("Get DOUBLE value\n\n");

    char att4[ ]="epsilon";
    iattlen = strlen(att4);
    fc_cmo_get_double_(cmo_name,att4,&xval,&ierr,icmolen,iattlen);
    printf("return epsilon: %g\n", xval);

    char att5[ ]="xmin";
    iattlen = strlen(att5);
    fc_cmo_get_double_(cmo_name,att5,&xval,&ierr,icmolen,iattlen);
    printf("return xmin: %f\n", xval);

    printf("------------------------------------------\n");
    printf("Get INT value\n\n");

    char att6[ ]="nnodes";
    iattlen = strlen(att6);
    fc_cmo_get_int_(cmo_name,att6,&ival,&ierr,icmolen,iattlen);
    printf("return nnodes: %ld\n", ival);

    printf("\n===== end cpp examples ==============================\n");
    return;

}
#ifdef __cplusplus
}
#endif

