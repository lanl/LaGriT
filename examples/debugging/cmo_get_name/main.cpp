#include <stdio.h>
#include <string.h>
#include <iostream>

using std::cout;
using std::endl;

#define LG_STR_LEN 80

extern "C" void cmo_get_name_(char*, int*, unsigned int);

void lg_trim(char* lg_str) {
    for (int i = 0; i < strlen(lg_str) - 1; ++i) {
        if (isspace(lg_str[i])) {
            lg_str[i] = '\0';
            break;
        }
    }
}

int main() {
    char cmo[LG_STR_LEN];
    int ierror;

    cmo_get_name_(cmo, &ierror, LG_STR_LEN);
    lg_trim(cmo);
    cout << "cmo: " << cmo << "; ierror = " << ierror << endl;

    return 0;
}
