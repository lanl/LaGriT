#include <stdio.h>
#include <string.h>
#include <iostream>

using std::cout;
using std::endl;

extern "C" void INITLAGRIT(char [], char [], char [], unsigned int, unsigned int, unsigned int);
extern "C" int DOTASK_C(char [], unsigned int);
extern "C" void CMO_GET_NAME(char [], int*, unsigned int);
extern "C" void CMO_GET_INFO(char [], char [], double**, int*, int*, int*, unsigned int, unsigned int);

int main() {
    char mode[] = "noisy"; // or "quiet"
    char log_file[] = " ";
    char batch_file[] = " ";
    char command1[] = "cmo/create/mo1///tet; finish";
    char cmo[] = "              ";
    char info_req[] = "ndimensions_topo";
    int ierror, itype, ilen;
    double *result;

    cout << "================" << endl;
    cout << "MODE = " << mode << endl;
    cout << "LOG_FILE = " << log_file << endl;
    cout << "BATCH_FILE = " << batch_file << endl << endl;
    cout << "================" << endl;

    INITLAGRIT(
        mode, log_file, batch_file,
        strlen(mode), strlen(log_file), strlen(batch_file)
    );

    ierror = 0;

    ierror = DOTASK_C(command1, strlen(command1));
    cout << "dotask returned with code: " << ierror << endl;

    CMO_GET_NAME(cmo, &ierror, strlen(cmo));
    cout << "cmo_get_name returned with code: " << ierror << endl;
    cout << "cmo name: " << cmo << endl;

    CMO_GET_INFO(info_req, cmo, &result, &ilen, &itype, &ierror, strlen(info_req), strlen(cmo));
    cout << "done" << endl;
    cout << "Result: " << result[0] << "; ilen: " << ilen << "; itype: " << itype << "; ierror: " << ierror << endl;

    return 0;
}