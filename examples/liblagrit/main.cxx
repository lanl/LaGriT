#include <iostream>
#include <string>
#include "lagrit.h"

int main() {
    std::string mode("noisy"); 
    std::string log_file(" ");
    std::string batch_file(" ");

    initlagrit(mode.c_str(), log_file.c_str(), batch_file.c_str(), mode.size(), log_file.size(), batch_file.size());
    return 0;
}
