/* Wrappers for fortran exodus routines
   Updates ExodusII 5.22a to 6.06 syntax
   Copied definitions from exodusII.h to this file to clarify argument definitions
*/

#include "exodusII.h"

/******************************************************************************/
/* ex_create
*
* int ex_create (path, cmode, comp_ws, io_ws);
* 
* char* path (R)
* The file name of the new EXODUS II file. This can be given as either an absolute path
* name (from the root of the file system) or a relative path name (from the current directory).
*
* int cmode (R)
* Mode. Use one of the following predefined constants:
*     EXNOCL (do not overwrite) EXCLOB (overwrite regardless)
*     EXNORM (32-bit offset)  EXLARG (larger than 2 gigabytes)
*     EXNET4 (HDF5-based netcdf-4)  EXNOSH (netCDF not in “share” mode)
*
* int* comp_ws (RW)
* The word size in bytes (0, 4 or 8) of the floating point variables used in the application
* program. If 0 (zero) is passed, the default sizeof(float) will be used and returned in this
* variable. WARNING: all EXODUS II functions requiring floats must be passed floats
* declared with this passed in or returned compute word size (4 or 8).
*
* int* io_ws (R)
* The word size in bytes (4 or 8) of the floating point data as they are to be stored in the
* EXODUS II file.
*
*/

int_ptrsize excre_wrapper(char *path,
                    int *clobmode,
                    int *cpu_word_size,
                    int *io_word_size)
{

	return ex_create(  path,
			clobmode,
			cpu_word_size,
			io_word_size)
}


/******************************************************************************/
/* ex_err
* void ex_err(const char *module_name, 
*             const char *message, 
*             int err_num);
*/
void ex_err_wrapper(const char* module_name, 
              const char* message, 
              int* err_num)
{
	ex_err(module_name, 
              message, 
              *err_num);
}

/******************************************************************************/
/*  Write Edge Face or Element Block Parameters */
/* ex_put_block
*   int ex_put_block(int exoid,
*                      ex_entity_type blk_type,
*                      ex_entity_id   blk_id,
*                      const char *entry_descrip,
*                      int64_t num_entries_this_blk,
*                      int64_t num_nodes_per_entry,
*                      int64_t num_edges_per_entry,
*                      int64_t num_faces_per_entry,
*                      int64_t num_attr_per_entry);
*/
void ex_put_block_wrapper(	int* exoid,  
			ex_entity_id* blk_id, 
			char* entry_descrip,
			int64_t* num_entries_this_blk, 
			int64_t* num_nodes_per_entry, 
			int64_t* num_edges, 
			int64_t* num_faces, 
			int64_t* num_attr_per_entry, 
			int* status)
{
	*status = ex_put_block(	*exoid, 
			EX_ELEM_BLOCK, 
			*blk_id, 
			entry_descrip, 
			*num_entries_this_blk, 
			*num_nodes_per_entry, 
			*num_edges, 
			*num_faces, 
			*num_attr_per_entry);
}

/******************************************************************************/
/*  Write Edge Face or Element Block Connectivity */
/* ex_put_conn
* int ex_put_conn(int exoid,
*                ex_entity_type blk_type,
*                ex_entity_id   blk_id,
*                const void_int *node_conn,
*                const void_int *elem_edge_conn,
*                const void_int *elem_face_conn);
*/ 

void ex_put_conn_wrapper(int* exoid, 
                 ex_entity_id* blk_id, 
                 const int* node_conn, 
                 int* status)
{
	//NOTE THE CAST FOR BLK_ID
	//This fixes a bug when passing this argument
	//to exodus that causes an error to be returned.
	//I have no explanation for why it works however.
	//     -Stephen Jenkins

	*status = ex_put_conn(*exoid, 
                  EX_ELEM_BLOCK, 
                  (int) *blk_id, 
                  node_conn, 
                  NULL, NULL);
}

/******************************************************************************/
/* ex_put_coord
* int ex_put_coord_names (int   exoid,
*                         char *coord_names[]);
*/

void ex_put_coord_wrapper(int* exoid, 
         void* x, 
         void* y, 
         void* z, 
         int* status)
{
	*status = ex_put_coord(*exoid, 
                                x, y, z);
}

/******************************************************************************/
/* ex_put_qa
* int ex_put_qa (int   exoid,
*                int   num_qa_records,
*                char* qa_record[][4]);
*/

void ex_put_qa_wrapper(int* exoid, 
                       int* num_qa_records, 
                       char* qa_record[][4], 
                       int* status)
{
	*status = ex_put_qa(*exoid, 
                            *num_qa_records, 
                            qa_record);
}

/******************************************************************************/
/*  Write Node Edge Face or Side Set Parameters */
/* ex_put_set_param
* int ex_put_set_param(int exoid,
*                      ex_entity_type set_type,
*                      ex_entity_id   set_id,
*                      int64_t num_entries_in_set,
*                      int64_t num_dist_fact_in_set);
*/

void ex_put_set_param_wrapper(int* exoid, 
                       ex_entity_id* set_id, 
                       int64_t* num_entries, 
                       int* status)
{
	*status = ex_put_set_param(*exoid, 
                       EX_NODE_SET, 
                       *set_id, 
                       *num_entries, 
                       0);
}

/******************************************************************************/
/*  Write a Node Edge Face or Side Set */
/* ex_put_set
* int ex_put_set(int exoid,
*                ex_entity_type set_type,
*                ex_entity_id   set_id,
*                const void_int *set_entry_list,
*                const void_int *set_extra_list);
*/
void ex_put_set_wrapper(int* exoid, 
                 ex_entity_id* set_id, 
                 void_int* set_entry_list, 
                 int* status)
{
	*status = ex_put_set(*exoid, 
                 EX_NODE_SET, 
                 *set_id, 
                 set_entry_list, 
                 NULL);
}

/******************************************************************************/
/* ex_put_side_set
* int ex_put_side_set (int   exoid,
*             ex_entity_id   side_set_id,
*           const void_int  *side_set_elem_list,
*           const void_int  *side_set_side_list);
*/
void ex_put_side_set_param_wrapper(int* exoid, 
            ex_entity_id* set_id, 
            int64_t* num_side_in_set, 
            int* status)
{
	*status = ex_put_side_set_param(*exoid, 
                  *set_id, 
                  *num_side_in_set, 
                  0);
}

/******************************************************************************/
/*  Write Node Edge Face or Side Set Parameters */
/* ex_put_set_param
* int ex_put_set_param(int exoid,
*                     ex_entity_type set_type,
*                     ex_entity_id   set_id,
*                     int64_t num_entries_in_set,
*                     int64_t num_dist_fact_in_set);
*/
void ex_put_set_wrapper(int* exoid, 
                      ex_entity_id* set_id, 
                      void_int* set_entry_list, 
                      int* status)
{
	*status = ex_put_set(*exoid, 
                      EX_NODE_SET, 
                      *set_id, 
                      set_entry_list, 
                      NULL);
}
/******************************************************************************/
/* ex_put_side_set_param
* int ex_put_side_set_param (int exoid,
*                     ex_entity_id side_set_id,
*                     int64_t num_side_in_set,
*                     int64_t num_dist_fact_in_set);
*/
void ex_put_side_set_param_wrapper(int* exoid, 
                      ex_entity_id* set_id, 
                      int64_t* num_side_in_set, 
                      int* status)
{
	*status = ex_put_side_set_param(*exoid, 
                      *set_id, 
                      *num_side_in_set, 
                      0);
}

/******************************************************************************/
/* ex_put_side_set
* int ex_put_side_set (int exoid,
*                     ex_entity_id   side_set_id,
*                     const void_int  *side_set_elem_list,
*                     const void_int  *side_set_side_list);
*/
void ex_put_side_set_wrapper(int* exoid, 
                     ex_entity_id* set_id, 
                     const void_int* elem_list, 
                     const void_int* side_list, 
                     int* status)
{
	*status = ex_put_side_set(*exoid,  
                     *set_id, 
                     elem_list, 
                     side_list);
}
/******************************************************************************/
/* ex_close
* int ex_close (int exoid);
*/
void exclos_wrapper(int* exoid, int* status)
{
	*status = ex_close(*exoid);
}

/******************************************************************************/

/* end file */
