/*
 *  exo_put_sets.c
 *  
 *
 *  Created by Quan Bui on 9/6/12.
 *  
 *
 */

#ifdef LAGRIT_INCLUDE_EXODUSII

#include "exodusII.h"
#include "type_sizes.h"
#include <string.h>
#include <stdio.h>
#include <assert.h>

#ifdef __cplusplus
extern "C" {
#endif

#define EXCHECK(funcall,errmsg)\
  if ( (funcall) < 0 ) \
    { \
      fprintf( stderr, errmsg ); \
    }

/****************************************************************
ExodusII v6 and later updated to 8 byte integers. see exodusII.h

typedef struct ex_set
{
  int64_t        id;
  ex_entity_type type;
  int64_t        num_entry;
  int64_t        num_distribution_factor;
  void_int *     entry_list;
  void_int *     extra_list;
  void *         distribution_factor_list;
} ex_set;

****************************************************************/

void exo_put_sets_(const int_ptrsize *idexo, const int_ptrsize *type, 
		   const char *name, const int_ptrsize *nlen, 
		   const int_ptrsize *sid, const int_ptrsize *nentry, 
		   const int_ptrsize *num_df,
		   const int_ptrsize *set_entry_list, int_ptrsize *status)
{
	// Declare an exodus set
	ex_set this_set;

        if (sizeof(*nlen) != sizeof(this_set.num_entry)) {
        printf("ERROR integer size mismatch for set array:\n");
        printf(" sizeof arg int: %lu  sizeof ex_set int: %lu \n",sizeof(*nlen),sizeof(this_set.num_entry));
        *status = -1;
        }

        // Verify that API expecting 8-byte integers 
        assert(ex_int64_status(*idexo) & EX_BULK_INT64_API); 

        // fill array for ex_set structure
	int_ptrsize set_array[*nentry];
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

	// Add the set to the data structure
	*status = ex_put_sets(*idexo, 1, &this_set);

	// Put set name
	char set_name[32];
	int_ptrsize name_len = *nlen;

  	strncpy(set_name, name, name_len);
        set_name[name_len] = '\0';

	EXCHECK( ex_put_name(*idexo, *type, *sid, set_name), "Unable to put set name!");

	printf("Done writing set no. %ld to ExodusII file \n", this_set.id);
}

#ifdef __cplusplus
}
#endif

#endif // LAGRIT_INCLUDE_EXODUSII
