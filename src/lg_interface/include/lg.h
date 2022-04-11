#pragma once

#ifdef __cplusplus
extern "C" {
#endif

typedef int LG_ERR; /// Error code returned by Lagrit functions

typedef enum LGInitMode {
    LG_INIT_QUIET,
    LG_INIT_NOISY
} LGInitMode;

typedef struct LGMeshStdPtrs {
    double* ipxic;
    double* ipyic;
    double* ipzic;
    int* ipimt1;
    int* ipitp1;
    int* ipicr1;
    int* ipisn1;
    int* ipitetclr;
    int* ipitettyp;
    int* ipitetoff;
    int* ipjtetoff;
    int* ipitet;
    int* ipjtet;
} LGMeshStdPtrs;

/// LaGriT error codes
/// (TODO: used as example; replace with your own error codes)
typedef enum LG_ERR_CODES {
    LG_ERR_CMO_NOT_FOUND = -1,
    /// Subroutine was successful
    LG_ERR_SUCCESS = 0,
    /// Unknown error occurred
    LG_ERR_UNKNOWN = 1,
    /// Invalid argument parameter
    LG_ERR_C_INVALID_ARGS = 2,
} LG_ERR_CODES;

/// Type of returned data
//typedef enum LGiType {
//    LG_ITYPE_INT = 0,
//    LG_ITYPE_REAL,
//    LG_ITYPE_CHAR,
//    LG_ITYPE_VINT,
//    LG_ITYPE_VDOUBLE,
//    LG_ITYPE_VCHAR,
//    LG_ITYPE_UNKNOWN
//} LGiType;
//
//typedef struct LGMeshInfo {
//    void* buffer;
//    unsigned int size;
//    LGiType type;
//
//    union {
//        int value_int;
//        double value_real;
//        char* value_vchar;
//        int* value_vint;
//        double* value_vdouble;
//    } data;
//} LGMeshInfo;

/// Initialize LaGriT
void LGInitialize(
    LGInitMode verbosity   /// [in] Initialize with 'noisy' or 'quiet' output
);

/// Send a LaGriT command for processing
LG_ERR LGSendCommand(
    const char* cmd); /// [in] LaGriT command

/// Get the name of the current mesh object (CMO)
LG_ERR lg_cmo_get_name(
    char* name_buffer,           /// [out] Buffer to store name
    const int name_buffer_size); /// [in]  Size of buffer

/// Gets a scalar integer mesh attribute and returns it to
/// the address pointed to by `result`
LG_ERR lg_cmo_get_intinfo(
    const char* ioption, // [in]
    const char* cmo_name, // [in]
    int* result); // [out]

//int lg_cmo_get_info();
//int lg_cmo_get_attinfo();
LG_ERR lg_cmo_get_stdptrs(
    const char* cmo_name,
    LGMeshStdPtrs* mesh_ptrs);

#ifdef __cplusplus
}
#endif
