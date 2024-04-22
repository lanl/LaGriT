/****************************************************************************
C++ Wrappers for common fortan routines

C++ prototype declarations must be extern "C" (do not use for C code)

Must match the Fortran and C++ types, for lagrit integers are 8 bytes 
So make sure all are 8 bytes for integer=long, real*8=double

All Fortran arguments are passed as pointers, whether they are input or output values.
Array names such as cmd_buffer are already pointers, so argument is cmd_buffer. 
but use & to pass values such as &size

Arrays in C and C++ index starts at 0. In Fortran the index starts at 1.

Fortran matrix element A(3,5) translates to C/C++ matrix element a[4][2]. 
(Subtract 1 for zero base indexing and reverse the order of the subscripts.) 

header file lg_c_interface.h has declarations for these lg_ wrappers
header file lg_f_interface has declarations for fortran DOTASK, CMO_GET_, etc
header file fc_mangle.h handles fortran name mangling during cmake 
       ie. define DOTASK FortranCInterface_GLOBAL(dotask, DOTASK)

NOTE: these only work for integer scalars 
      Use f90 wrappers that can pass pointers in lg_fc_wrappers.f90

****************************************************************************/

#include "lg_c_interface.h"
#include "lg_f_interface.h"
#include "type_sizes.h"
#include <stdio.h>
#include <cstring>
#include <iostream>

using std::cout;
using std::endl;

// Maximum buffer size for stack-allocated buffers
static constexpr size_t MAX_BUFFER_SIZE = 1024;

extern "C"
void lg_initlagrit() {
    const char* mode = "noisy";
    const char* log_file = " ";
    const char* batch_file = " ";

    INITLAGRIT(
        mode,
        log_file,
        batch_file,
        strlen(mode),
        strlen(log_file),
        strlen(batch_file));
}

extern "C"
LG_ERR lg_dotask(const char* cmd) {
    const char* cmd_finish = "; finish";



    if (strlen(cmd) >= (MAX_BUFFER_SIZE - strlen(cmd_finish))) {
        return LG_ERR_C_INVALID_ARGS;
    }

    char cmd_buffer[MAX_BUFFER_SIZE];

    int result = snprintf(
        cmd_buffer,
        MAX_BUFFER_SIZE,
        "%s%s",
        cmd,
        cmd_finish);

    if ((result >= 0) && (result < MAX_BUFFER_SIZE)) {
        int_ptrsize err = 0;
        DOTASK(cmd_buffer, &err, (size_t)strlen(cmd_buffer));
        return (LG_ERR)err;
    } else {
        return LG_ERR_C_INVALID_ARGS;
    }
}

// test strings passed from cpp to fortran
// similar to dotask but do not process strings as commands
extern "C"
LG_ERR lg_dotask_test(const char* cmd) {
    const char* cmd_finish = "; finish";

//      arg values passed to fortran should be size 8
//      int_ptrsize is usually integer 8 same as long
//      The hidden string length is passed as size_t as 8 bytes on x64

    int_ptrsize err = 0;
    size_t ival = 0;

    printf("Inside C wrapper lg_dotask_test \n");
    printf("received string: %s\n", cmd);
    printf("string length: %ld\n", strlen(cmd));

    if (strlen(cmd) >= (MAX_BUFFER_SIZE - strlen(cmd_finish))) {
        return LG_ERR_C_INVALID_ARGS;
    }

    char cmd_buffer[MAX_BUFFER_SIZE];

    int result = snprintf(
        cmd_buffer,
        MAX_BUFFER_SIZE,
        "%s%s",
        cmd,
        cmd_finish);

    if ((result >= 0) && (result < MAX_BUFFER_SIZE)) {

        printf("  sizeof strlen: %ld\n",sizeof(strlen(cmd_buffer)));
        printf("  sizeof err: %ld\n",sizeof(err));
        printf("  sizeof hidden length: %ld\n",sizeof(ival));


        printf("sending parameters to FORTRAN dotask_test\n");
        printf("send string: %s\n", cmd_buffer);
        printf("string length: %ld\n", strlen(cmd_buffer));
        
        DOTASK_TEST(cmd_buffer, &err, strlen(cmd_buffer));

        return (LG_ERR)err;

    } else {
        return LG_ERR_C_INVALID_ARGS;
    }
}

extern "C"
LG_ERR lg_cmo_get_name(char* name_buffer, int name_buffer_size) {
    int_ptrsize err = 0;

    if (name_buffer == nullptr) {
        return LG_ERR_C_INVALID_ARGS;
    }

    char buffer[MAX_BUFFER_SIZE];
    CMO_GET_NAME(buffer, &err, MAX_BUFFER_SIZE);

    if (err == 0) {
        size_t len = 0;
        size_t max_len = name_buffer_size < MAX_BUFFER_SIZE ? name_buffer_size : MAX_BUFFER_SIZE;

        // Fortran will pad strings with spaces, so copy until we hit a space
        while ((len < max_len - 1) && (buffer[len] != ' ')) { ++len; }
        snprintf(name_buffer, len + 1, "%s", buffer);
    }

    return (LG_ERR)err;
}

/***********************************************************************
lg_cmo_get_intinfo
Wrapper for fortran subroutine cmo_get_intinfo 

Returns integer scalar from mesh object cmo_name

***********************************************************************/


extern "C"
int lg_cmo_get_intinfo(const char* ioption, const char* cmo_name) {
    int_ptrsize err, data, size, type;

    // size and type returned data can be ignored
    // maybe int_ptrsize data with arg &data and return data
    // maybe int_ptrsize data[1] with arg data and return data[0]

    int ioptlen = strlen(ioption);
    int imolen = strlen(cmo_name);
    data = 0;
    size = 0;
    type = 0;
    err  = 0;

/* DEBUG
    printf("CXX lg_cmo_get_intinfo: '%s'\n", ioption);
    printf("CMO: '%s'\n", cmo_name);
    printf("ioptlen and cmo len: %d %d\n", ioptlen, imolen);
    printf("atdata, data: %d, %d\n", &data,data);
    printf("atsize, size: %d, %d\n", &size,size);
*/

    CMO_GET_INTINFO(
        ioption,
        cmo_name,
        &data,
        &size, 
        &type, 
        &err,
        ioptlen,
        imolen);
    
    if (err != 0) {
        printf("Error lg_cmo_get_intinfo: %d\n", err);
    }

/* DEBUG
    printf("exit atdata, data: %d, %d\n", &data,data);
    printf("exit atsize, size: %d, %d\n", &size,size);
*/

    return data;
}

