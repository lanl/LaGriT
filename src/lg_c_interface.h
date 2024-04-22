#pragma once

/*
 * C WRAPPER DECLARATIONS and CONSTANTS 
 * C code header file
 * These are C wrappers for lagrit fortran commands
 * as declared in lg_f_interface.h
 *
 * This header file is used for exposing
 * lagrit fortran subroutines to C codes.
 * Names with CAPS allows name binding
 * as defined in fc_mangle.h
*/


#ifdef __cplusplus
extern "C" {
#endif

typedef int LG_ERR; /// Error code returned by Lagrit functions

/// LaGriT error codes
/// (TODO: used as example; replace with your own error codes)

typedef enum LG_ERR_CODES : LG_ERR {
    /// Subroutine was successful
    LG_ERR_SUCCESS = 0,
    /// Unknown error occurred
    LG_ERR_UNKNOWN = 1,
    /// Invalid argument parameter
    LG_ERR_C_INVALID_ARGS = 2,
} LG_ERR_CODES;

/// Type of returned data
typedef enum LGiType : int {
    LG_ITYPE_INT = 0,
    LG_ITYPE_REAL,
    LG_ITYPE_CHAR,
    LG_ITYPE_VINT,
    LG_ITYPE_VDOUBLE,
    LG_ITYPE_VCHAR,
    LG_ITYPE_UNKNOWN
} LGiType;

typedef struct LGMeshInfo {
    void* buffer;
    unsigned int size;
    LGiType type;

    union {
        int value_int;
        double value_real;
        char* value_vchar;
        int* value_vint;
        double* value_vdouble;
    } data;
} LGMeshInfo;

/// Initialize LaGriT
void lg_initlagrit();

/// Send a LaGriT command for processing
LG_ERR lg_dotask(const char* cmd);
LG_ERR lg_dotask_test(const char* cmd);

LG_ERR lg_cmo_get_name(char* name_buffer, int name_buffer_size);

/// Get mesh object attribute integer value 
int lg_cmo_get_intinfo(const char* ioption, const char* cmo_name);

/// Get mesh object attribute double value 
void lg_cmo_get_int(const char* ioption, const char* cmo_name, long ival);
void lg_cmo_get_vint(const char* ioption, const char* cmo_name, long* ival);

/// Get mesh object attribute scalar or pointer based on size and type 
void lg_cmo_get_attinfo(const char* ioption, const char* cmo_name,
     int iout, double rout, char* cout,
     void* ipout, int size, int type);


#ifdef __cplusplus
}
#endif

