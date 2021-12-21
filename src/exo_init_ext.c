/*
 *  exo_init_ext.c
 *  
 *
 *  Created by Quan Bui on 9/6/12.
 *  
 *
 */

#ifdef LAGRIT_INCLUDE_EXODUSII

#include "exodusII.h"
#include <string.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

void exo_lg_ini_(int *idexo, void_int *ndim,
		 void_int *nnodes, void_int *nelements, void_int *nelem_blk,
		 void_int *nnode_sets, void_int *nside_sets,
		 void_int *nelem_sets, int *status)
{
	ex_init_params params;

	int64_t *n_dim = ndim;
	int64_t *n_nodes = nnodes;
	int64_t *n_elements = nelements;
	int64_t *n_elem_blk = nelem_blk;
	int64_t *n_node_sets = nnode_sets;
	int64_t *n_side_sets = nside_sets;
	int64_t *n_elem_sets = nelem_sets;

	strcpy(params.title, "LaGriT to Exodus");
	params.num_dim = *n_dim;
	params.num_nodes = *n_nodes;
	params.num_elem = *n_elements;
	params.num_edge = 0;
	params.num_face = 0;
	params.num_edge_blk = 0;
	params.num_face_blk = 0; 
	params.num_elem_blk = *n_elem_blk;
	params.num_node_sets = *n_node_sets;
	params.num_edge_sets = 0;
	params.num_side_sets = *n_side_sets;
	params.num_face_sets = 0;
	params.num_elem_sets = *n_elem_sets;
	params.num_node_maps = 0;
	params.num_edge_maps = 0;
	params.num_face_maps = 0;
	params.num_elem_maps = 0;

/* commented out since this is already written to screen and log
	printf("Title: %s \n", params.title);
	printf("number of dimension: %d \n", params.num_dim);
	printf("number of nodes: %d \n", params.num_nodes);
	printf("number of elements: %d \n", params.num_elem);
	printf("number of edges: %d \n", params.num_edge);
	printf("number of edge blocks: %d \n", params.num_edge_blk);
	printf("number of element blocks: %d \n", params.num_elem_blk);
	printf("number of face blocks: %d \n", params.num_face_blk);
	printf("number of node sets: %d \n", params.num_node_sets);
	printf("number of edge sets: %d \n", params.num_edge_sets);
	printf("number of element sets: %d \n", params.num_elem_sets);
	printf("number of side sets: %d \n", params.num_side_sets);
	printf("number of face sets: %d \n", params.num_face_sets);
	printf("number of node maps: %d \n", params.num_node_maps);
	printf("number of edge maps: %d \n", params.num_edge_maps);
	printf("number of face maps: %d \n", params.num_face_maps);
	printf("number of element maps: %d \n", params.num_elem_maps);
	printf("\n");
*/
	
	*status = ex_put_init_ext(*idexo, &params);
}
#ifdef __cplusplus
}
#endif

#endif // LAGRIT_INCLUDE_EXODUSII
