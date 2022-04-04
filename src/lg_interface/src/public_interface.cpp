#include <lagrit.h>
#include <lg_interface.h>

#include <stdio.h>
#include <cstring>

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
        DOTASK(cmd_buffer, &err, strlen(cmd_buffer));
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

extern "C"
int lg_cmo_get_intinfo(const char* ioption, const char* cmo_name) {
    int_ptrsize err, size, type;
    int_ptrsize data[1];

    CMO_GET_INTINFO(
        ioption,
        cmo_name,
        data,
        &size,
        &type,
        &err,
        strlen(ioption),
        strlen(cmo_name));
    
    if (err != 0) {
        printf("Error: %d\n", err);
    }

    return data[0];
}
