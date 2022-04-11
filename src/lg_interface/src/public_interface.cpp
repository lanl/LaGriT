#include <lg.h>
#include <lg_interface.h>

#include <stdio.h>
#include <cstring>

// Maximum buffer size for stack-allocated buffers
static constexpr size_t MAX_BUFFER_SIZE = 1024;

extern "C"
void LGInitialize(LGInitMode verbosity) {
    char mode[MAX_BUFFER_SIZE];
    const char* log_file = " ";
    const char* batch_file = " ";

    if (verbosity == LG_INIT_NOISY) {
        snprintf(mode, MAX_BUFFER_SIZE, "noisy");
    } else {
        snprintf(mode, MAX_BUFFER_SIZE, "quiet");
    }

    INITLAGRIT(
        mode,
        log_file,
        batch_file,
        strlen(mode),
        strlen(log_file),
        strlen(batch_file));
}

extern "C"
LG_ERR LGSendCommand(const char* cmd) {
    char cmd_buffer[MAX_BUFFER_SIZE];
    const char* cmd_finish = "; finish";

    // Ensure the command will still fit in our final command buffer
    // Command buffer needs to fit:
    //   - cmd + "; finish" + null terminator
    if (strlen(cmd) >= (MAX_BUFFER_SIZE - strlen(cmd_finish) - 1)) {
        return LG_ERR_C_INVALID_ARGS;
    }

    int result = snprintf(
        cmd_buffer,
        MAX_BUFFER_SIZE,
        "%s%s",
        cmd,
        cmd_finish);
    
    if ((result >= 0) && (result < MAX_BUFFER_SIZE)) {
        int_ptrsize err = 0;
        DOTASK(cmd_buffer, &err, strlen(cmd_buffer));
        return (LG_ERR)err;
    } else {
        return LG_ERR_C_INVALID_ARGS;
    }
}

extern "C"
LG_ERR lg_cmo_get_name(char* name_buffer, int name_buffer_size) {
    int_ptrsize err = 0;
    char buffer[MAX_BUFFER_SIZE];

    if (name_buffer == nullptr) {
        return LG_ERR_C_INVALID_ARGS;
    }

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

extern "C"
LG_ERR lg_cmo_get_intinfo(const char* ioption, const char* cmo_name, int* info) {
    int_ptrsize data, err;

    CMO_GET_INTINFO(
        ioption,
        cmo_name,
        &data,
        &err,
        strlen(ioption),
        strlen(cmo_name));
    
    if (err == 0) {
        *info = data;
    }

    return (LG_ERR)err;
}

// int_ptrsize** ipimt1,
// int_ptrsize** ipitp1,
// int_ptrsize** ipicr1,
// int_ptrsize** ipisn1,
// double** ipxic,
// double** ipyic,
// double** ipzic,
// int_ptrsize** ipitetclr,
// int_ptrsize** ipitettyp,
// int_ptrsize** ipitetoff,
// int_ptrsize** ipjtetoff,
// int_ptrsize** ipitet,
// int_ptrsize** ipjtet,

extern "C"
LG_ERR lg_cmo_get_stdptrs(const char* cmo_name, LGMeshStdPtrs* mesh_ptrs) {
    int_ptrsize ierr = LG_ERR_CMO_NOT_FOUND;
    printf("hello! %s\n", cmo_name);

    CMO_GET_STDPTRS(
        cmo_name,
        mesh_ptrs->ipimt1,
        mesh_ptrs->ipitp1,
        mesh_ptrs->ipicr1,
        mesh_ptrs->ipisn1,
        mesh_ptrs->ipxic,
        mesh_ptrs->ipyic,
        mesh_ptrs->ipzic,
        mesh_ptrs->ipitetclr,
        mesh_ptrs->ipitettyp,
        mesh_ptrs->ipitetoff,
        mesh_ptrs->ipjtetoff,
        mesh_ptrs->ipitet,
        mesh_ptrs->ipjtet,
        &ierr,
        strlen(cmo_name)
    );

    printf("back from cmd\n");
    printf("imt1 -----> %d\n", mesh_ptrs->ipimt1[0]);

    printf("ierr = %d\n", ierr);

    return LG_ERR_SUCCESS;
}