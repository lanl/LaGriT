/*
 * This is a wrapper function for the Exodus function excre_. That function
 * takes an integer path length, which is hard (or impossible?) to pass from
 * Fortran, since Fortran always passes pointers.
 */

#include "type_sizes.h"


int excre_(char *path,
        int *clobmode,
        int *cpu_word_size,
        int *io_word_size,
        int *ierr,
        int pathlen);

int_ptrsize excre_wrapper_(char *path,
        int_ptrsize *clobmode,
        int_ptrsize *cpu_word_size,
        int_ptrsize *io_word_size,
        int_ptrsize *ierr,
        int_ptrsize *pathlen)
{
	/*
    return excre_(path,
            (int *)clobmode,
            (int *)cpu_word_size,
            (int *)io_word_size,
            (int *)ierr,
            (int)(*pathlen));
	*/
	return 0;
}
