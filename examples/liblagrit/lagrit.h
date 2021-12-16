#ifndef __LAGRIT_LIB_HPP__
#define __LAGRIT_LIB_HPP__

/* Initializes LaGriT */
extern "C" void initlagrit_(const char * mode,
                            const char* log_file,
                            const char * batch_file,
                            unsigned int len_mode,
                            unsigned int len_log_file,
                            unsigned int len_batch_file);

extern "C" void dotask_(const char* cmd, unsigned int len_cmd);

#endif