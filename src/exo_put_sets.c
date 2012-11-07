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
      return 1; \
    }

void exo_put_sets_(int_ptrsize *idexo, int_ptrsize *type, char *name, int_ptrsize *sid,
		   int_ptrsize *nentry, int_ptrsize *num_df,
		   int_ptrsize *set_entry_list, int_ptrsize *status)
{

	ex_set this_set;

	int_ptrsize *this_type = type;
	int_ptrsize *this_id = sid;
	int_ptrsize *this_nentry = nentry;
	int_ptrsize *this_df = num_df;
	int_ptrsize *set_ptr = set_entry_list;

	/* NETCDF restricts the variables and record to 4-byte boundaries
	We have to create a 4-bye integer array and fill itinstead of 
	passing the pointer directly */

	int set_array[*this_nentry];
	int_ptrsize i;
	for (i=0; i<*this_nentry; i++){
		set_array[i] = *(set_ptr+i);
	}

	// Initialize the set structure
	this_set.type = *this_type;
	this_set.id  = *this_id;
	this_set.num_entry = *this_nentry;
	this_set.num_distribution_factor = *this_df;
	this_set.entry_list = &set_array;
	this_set.extra_list = NULL;
	this_set.distribution_factor_list  = NULL;

	// Put the set to the data structure
	*status = ex_put_sets(*idexo, 1, &this_set);
	// Put set name
	EXCHECK( ex_put_name(*idexo, *this_type, *this_id, name), "Unable to put set name!");

	printf("Done writing set no. %d to exodus file \n", this_set.id);
}

#ifdef __cplusplus
}
#endif
