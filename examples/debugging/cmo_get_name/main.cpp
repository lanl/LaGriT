#include <stdio.h>
#include <string>
#include <iostream>
#include <exception>

using std::cout;
using std::endl;

extern "C" void initlagrit_(char [], char [], char [], unsigned int, unsigned int, unsigned int);
extern "C" void cmo_get_name_(char*, int*, unsigned int);
extern "C" void dotask_(char*, int*, unsigned int);

namespace LaGriT {
    const int STR_LEN = 80;

    struct LGRuntimeError : public std::exception {
        const char * what() const throw() {
            return "LaGriT Runtime Error";
        }
    };

    void init(std::string mode = "noisy", std::string log_file = " ", std::string batch_file = " ") {
        char mode_c[mode.length() + 1];
        char log_file_c[log_file.length() + 1];
        char batch_file_c[batch_file.length() + 1];

        strcpy(mode_c, mode.c_str());
        strcpy(log_file_c, log_file.c_str());
        strcpy(batch_file_c, batch_file.c_str());

        initlagrit_(mode_c, log_file_c, batch_file_c, strlen(mode_c), strlen(log_file_c), strlen(batch_file_c));
    }

    void lg_trim(char* lg_str) {
        for (int i = 0; i < strlen(lg_str) - 1; ++i) {
            if (isspace(lg_str[i])) {
                lg_str[i] = '\0';
                break;
            }
        }
    }

    std::string cmo_get_name() {
        char cmo[STR_LEN];
        int ierror;
        cmo_get_name_(cmo, &ierror, STR_LEN);

        if (ierror != 0) {
            throw LGRuntimeError();
        }

        lg_trim(cmo);

        std::string str(cmo);
        return str;
    }

    void dotask(std::string task) {
        int ierror;
        char c_task[task.length() + 1];
        strcpy(c_task, task.c_str());
        dotask_(c_task, &ierror, strlen(c_task));

        if (ierror != 0) {
            throw LGRuntimeError();
        }
    }
}


int main() {
    LaGriT::init();
    std::string cmo = LaGriT::cmo_get_name();
    cout << "cmo ==>" << cmo << "<==" << endl;

    LaGriT::dotask("cmo/create/mo1///tri");

    //cmo_get_name_(cmo, &ierror, LG_STR_LEN);
    //LaGriT::lg_trim(cmo);
    //cout << "cmo: " << cmo << "; ierror = " << ierror << endl;

    //do_task_(task, ierror, );

    return 0;
}
