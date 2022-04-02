#include <lagrit.h>
#include <lg_interface.h>

#include <stdio.h>
#include <cstring>

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
