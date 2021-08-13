#include <stdio.h>
#include <string.h>
#include <iostream>

using std::cout;
using std::endl;

extern "C" void INITLAGRIT(char [], char [], char [], unsigned int, unsigned int, unsigned int);
extern "C" int DOTASK_C(char [], unsigned int);
extern "C" int CMO_GET_NAME_C(char [], unsigned int);
//extern "C" void CMO_GET_INFO(char [], char [], double**, int*, int*, int*, unsigned int, unsigned int);
extern "C" int CMO_GET_INFO_C(char [], char [], int*, int*, unsigned int, unsigned int);

int main() {
    char mode[] = "noisy"; // or "quiet"
    char log_file[] = " ";
    char batch_file[] = " ";
    char command1[] = "cmo/create/mo1///tet; finish";
    char info_req[] = "ndimensions_topo";
    int ierror = 0;
    int itype, ilen;
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

    char cmo[20];
    ierror = CMO_GET_NAME_C(cmo, 20);
    cout << "cmo name: " << cmo << endl;

    ierror = CMO_GET_INFO_C(info_req, cmo, &ilen, &itype, strlen(info_req), strlen(cmo));
    cout << "done" << endl;
    cout << "Get \"" << info_req << "\" from cmo \"" << cmo << "\"\n";
    //cout << "ilen: " << ilen << "; itype: " << itype << "; ierror: " << ierror << endl;
    //cout << "Result: " << result[0] << "; ilen: " << ilen << "; itype: " << itype << "; ierror: " << ierror << endl;

    return 0;
}