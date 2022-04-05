#pragma once

#ifdef __cplusplus
extern "C" {
#endif

//        iout    - (integer)  data to be returned.
//        lout    - (integer)  length of the data to be returned.
//        itype   - (integer) type of the data to be returned 
//                  (INT=1 or REAL=2) 
//                   Otherwise -1 for CHARACTER, VINT, VDOUBLE, VCHAR
// 
//        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error).
//

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

LG_ERR lg_cmo_get_name(char* name_buffer, int name_buffer_size);

/// Gets a scalar integer mesh attribute and returns it to
/// the address pointed to by `result`
int lg_cmo_get_intinfo(const char* ioption, const char* cmo_name);

#ifdef __cplusplus
}
#endif
