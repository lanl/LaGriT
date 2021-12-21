/*
 * Simple example illustrating linking LaGriT
 * with an external C/C++ code.
 */

#include <iostream>
#include <string>
#include "lagrit.h"

int main() {
    std::string mode("noisy"); 
    std::string log_file(" ");
    std::string batch_file(" ");

    std::string task("cmo/create/mo1///tri; finish");

    initlagrit(mode.c_str(), log_file.c_str(), batch_file.c_str(), mode.size(), log_file.size(), batch_file.size());
    dotask(task.c_str(), task.size());
    return 0;
}
