/*
 *  exo_put_sets.c
 *  
 *
 *  Created by Quan Bui on 9/6/12.
 *  
 *
 */

#include "exodusII.h"
#include "type_sizes.h"
#include <string.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#define EXCHECK(funcall,errmsg)\
  if ( (funcall) < 0 ) \
    { \
      fprintf( stderr, errmsg ); \
    }

void exo_put_sets_(const int_ptrsize *idexo, const int_ptrsize *type, 
		   const char *name, const int_ptrsize *nlen, 
		   const int_ptrsize *sid, const int_ptrsize *nentry, 
		   const int_ptrsize *num_df,
		   const int_ptrsize *set_entry_list, int_ptrsize *status)
{
	// Declare an exodus set
	ex_set this_set;

	/* NETCDF restricts the variables and record to 4-byte boundaries
	We have to create a 4-bye integer array and fill itinstead of 
	passing the pointer directly */

	int set_array[*nentry];
	int_ptrsize i;
	for (i=0; i<*nentry; i++){
		set_array[i] = *(set_entry_list+i);
	}

	// Initialize the set structure
	this_set.type = *type;
	this_set.id  = *sid;
	this_set.num_entry = *nentry;
	this_set.num_distribution_factor = *num_df;
	this_set.entry_list = &set_array;
	this_set.extra_list = NULL;
	this_set.distribution_factor_list  = NULL;

	// Put the set to the data structure
	*status = ex_put_sets(*idexo, 1, &this_set);

	// Put set name
	char set_name[32];
	int name_len = *nlen;
	strncpy(set_name, name, name_len);

	EXCHECK( ex_put_name(*idexo, *type, *sid, set_name), "Unable to put set name!");

	printf("Done writing set no. %d to exodus file \n", this_set.id);
}

#ifdef __cplusplus
}
#endif
